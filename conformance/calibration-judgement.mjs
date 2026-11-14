// Replays conformance/vectors/calibration-judgement.json against
// Reef.Calibration.Judge. The vectors are DeepStar's own answers on fixed
// inputs, recorded by its TestJudgementOracle before it was retired; the
// judgement in Reef must give the same. Exit 0 when every case agrees.
//
//   node conformance/calibration-judgement.mjs      after `spago build -p reef-stored`
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
const here = (path) => fileURLToPath(new URL(path, import.meta.url));
const vectors = JSON.parse(readFileSync(here("vectors/calibration-judgement.json"), "utf8"));
if (vectors["calibration-judgement-vectors"] !== 1) throw new Error("a vector format this replayer does not understand");
const Judge = await import(here("../output/Reef.Calibration.Judge/index.js"));
const Maybe = await import(here("../output/Data.Maybe/index.js"));
const just = (value) => new Maybe.Just(value);
const nothing = Maybe.Nothing.value;
const maybe = (value) => (value === undefined ? nothing : just(value));
const near = (a, b, tolerance) => Math.abs(a - b) <= tolerance;
const cents = vectors.toleranceCents, hertz = vectors.toleranceHz;
const failures = [];
const fail = (section, name, why) => failures.push(`${section}/${name}: ${why}`);
let count = 0;
const each = (section, run) => {
  const cases = vectors[section];
  if (!Array.isArray(cases) || cases.length === 0) throw new Error(`no ${section} cases: nothing would be tested`);
  for (const testCase of cases) { count++; run(testCase); }
};

each("readings", (c) => {
  const got = Judge.combine(c.hz);
  if (!near(got.median, c.median, hertz) || !near(got.spreadCents, c.spreadCents, cents) || !near(got.agreementCents, c.agreementCents, cents))
    fail("readings", c.name, `${JSON.stringify(got)}, DeepStar gave ${c.median} ${c.spreadCents} ${c.agreementCents}`);
});

each("snap", (c) => {
  const got = Judge.snap(c.hz)(c.expect)(c.toleranceCents);
  if (!near(got.folded, c.folded, hertz) || got.octaves !== c.octaves || got.ok !== c.ok)
    fail("snap", c.name, `${JSON.stringify(got)}, DeepStar gave ${c.folded} ${c.octaves} ${c.ok}`);
});

each("base", (c) => {
  const got = Judge.baseOf(c.hz);
  if (got.note !== c.note || !near(got.offsetCents, c.offsetCents, cents)) fail("base", c.name, `${JSON.stringify(got)}, DeepStar gave ${c.note} ${c.offsetCents}`);
});

const point = (p) => ({
  volts: p.volts, hz: p.hz, errCents: maybe(p.err_cents), reads: maybe(p.reads), spreadCents: maybe(p.spread_cents),
  agreementCents: maybe(p.agreement_cents), octaveShift: maybe(p.octave_shift),
});

each("problems", (c) => {
  const got = Judge.problems(c.points.map(point)).map(Judge.problemKind);
  if (JSON.stringify(got) !== JSON.stringify(c.kinds)) fail("problems", c.name, `${JSON.stringify(got)}, DeepStar gave ${JSON.stringify(c.kinds)}`);
});

each("heard", (c) => {
  const table = {
    label: c.name, points: c.hz.map((hz, step) => point({ volts: step, hz })), bus: 8, channel: 0, coarseSetting: nothing,
    baseNote: nothing, baseOffsetCents: nothing, refHzAt0V: c.refHz > 0 ? just(c.refHz) : nothing, module: nothing, device: nothing,
    measuredAt: nothing, probe: nothing, stimulus: nothing, fullScaleVolts: nothing, resolutionCents: nothing, role: nothing,
    sampleRate: nothing, usableHz: nothing, octaveSnapCents: nothing, octaveOffset: nothing, derivedFrom: nothing,
  };
  const got = Judge.heard(table)(c.heardHz);
  const refused = !(got instanceof Judge.Moved);
  if (refused !== c.refused) return fail("heard", c.name, `refused ${refused}, DeepStar ${c.refused}`);
  if (refused) return;
  const moved = got.value1;
  if (got.value0 !== c.octaves) return fail("heard", c.name, `${got.value0} octaves, DeepStar ${c.octaves}`);
  if (!moved.points.every((p, i) => near(p.hz, c.shiftedHz[i], hertz))) fail("heard", c.name, `pitches ${moved.points.map((p) => p.hz)}, DeepStar ${c.shiftedHz}`);
  if (c.octaves !== 0) {
    const note = moved.baseNote instanceof Maybe.Just ? moved.baseNote.value0 : "";
    const offset = moved.baseOffsetCents instanceof Maybe.Just ? moved.baseOffsetCents.value0 : NaN;
    const ref = moved.refHzAt0V instanceof Maybe.Just ? moved.refHzAt0V.value0 : NaN;
    if (note !== c.baseNote || !near(offset, c.baseOffsetCents, cents) || !near(ref, c.shiftedRefHz, hertz))
      fail("heard", c.name, `base ${note} ${offset} ${ref}, DeepStar ${c.baseNote} ${c.baseOffsetCents} ${c.shiftedRefHz}`);
  }
});

for (const failure of failures) console.log("FAIL  " + failure);
console.log(`calibration judgement against DeepStar's answers: ${count} cases, ${failures.length} failures`);
process.exit(failures.length === 0 ? 0 : 1);
