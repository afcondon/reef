#!/usr/bin/env node
// Melichthys by the terminal: Odonus's own heads (Reef.Odonus.stepEmit) over a
// generated line (Reef.Melody), printed as a Tidal block for Limulus.
//
//   node tools/melichthys.mjs [--seed N] [--bars N] [--scale NAME] [--root PC]
//                             [--heads "SPEC, SPEC, …"] [--play BARS] [--sound NAME]
//
// A head SPEC is words in any order:
//   a speed       1/4 1/3 1/2 2/3 1 3/2 2 3 4   (augmentation and diminution)
//   fwd|back|pend its direction                  (back: the retrograde)
//   inv           upside down                    (the inversion)
//   +N | -N       scale steps up or down         (+4: the answer at the fifth)
//   @N            comes in after N bars
// Up to four heads; default: four forward at 1, a quarter of the line apart,
// at 0, +4, -7 and -3. Run `spago build` in reef first.
import { generate, flatten, lineSteps, planText, lineText } from "../output/Reef.Melody/index.js";
import { seedFrom } from "../output/Reef.Marbles/index.js";
import { defaultOdonus, stepEmit } from "../output/Reef.Odonus/index.js";
import { load, enter } from "../output/Reef.Melichthys/index.js";

const arg = (name, fallback) => {
  const i = process.argv.indexOf("--" + name);
  return i > 0 ? process.argv[i + 1] : fallback;
};
const seed = parseInt(arg("seed", "1"), 10);
const bars = parseInt(arg("bars", "16"), 10);
const scaleName = arg("scale", "minor");
const root = parseInt(arg("root", "0"), 10);
const sound = arg("sound", "superpiano");
const scales = {
  major: [0, 2, 4, 5, 7, 9, 11], minor: [0, 2, 3, 5, 7, 8, 10], dorian: [0, 2, 3, 5, 7, 9, 10],
  phrygian: [0, 1, 3, 5, 7, 8, 10], lydian: [0, 2, 4, 6, 7, 9, 11], mixolydian: [0, 2, 4, 5, 7, 9, 10],
  harmonic: [0, 2, 3, 5, 7, 8, 11],
};
if (!scales[scaleName]) { console.error(`no scale '${scaleName}' (${Object.keys(scales).join(", ")})`); process.exit(1); }

const rates = ["1/4", "1/3", "1/2", "2/3", "1", "3/2", "2", "3", "4"];   // Reef.Odonus.rateTable
const quarter = Math.max(1, Math.round(bars / 4));
const specText = arg("heads", `1 fwd, 1 fwd +4 @${quarter}, 1 fwd -7 @${2 * quarter}, 1 fwd -3 @${3 * quarter}`);
const specs = specText.split(",").map((t) => {
  const spec = { speedIx: 4, direction: 0, invert: false, degrees: 0, enterBars: 0 };
  for (const w of t.trim().split(/\s+/).filter(Boolean)) {
    if (rates.includes(w)) spec.speedIx = rates.indexOf(w);
    else if (w === "fwd") spec.direction = 0;
    else if (w === "back") spec.direction = 1;
    else if (w === "pend") spec.direction = 2;
    else if (w === "inv") spec.invert = true;
    else if (/^[+-]\d+$/.test(w)) spec.degrees = parseInt(w, 10);
    else if (/^@\d+$/.test(w)) spec.enterBars = parseInt(w.slice(1), 10);
    else { console.error(`head word '${w}' not understood`); process.exit(1); }
  }
  return spec;
}).slice(0, 4);
const ratio = (ix) => { const [n, d] = rates[ix].split("/").map(Number); return { n, d: d || 1 }; };

const { melody } = generate(bars)(seedFrom(seed));
const line = flatten(melody);
const steps = lineSteps(line);

// the machine: the line loaded, the key set, the heads as asked, then entered
let odo = load(line)({ ...defaultOdonus, rootPc: root, scaleIvls: scales[scaleName] });
odo = { ...odo, heads: odo.heads.map((h, i) => {
  const s = specs[i];
  return s ? { ...h, mute: false, speedIx: s.speedIx, direction: s.direction, invert: s.invert, transp: 0, spreadOct: 0, spreadDeg: s.degrees }
           : { ...h, mute: true };
}) };
// a wait is counted in the head's own ticks: bars × 16 steps × its speed
odo = enter(specs.map((s) => { const r = ratio(s.speedIx); return Math.round((s.enterBars * 16 * r.n) / r.d); }))(odo);

const lastEntry = Math.max(...specs.map((s) => s.enterBars));
const playBars = parseInt(arg("play", String(lastEntry + bars)), 10);
const TICKS = 12;   // a step in twelfths: halves, thirds and quarters of it are whole
const events = specs.map(() => []);
for (let step = 0; step < playBars * 16; step++) {
  const r = stepEmit(odo);
  odo = r.odo;
  for (const f of r.fired) {
    const rate = ratio(odo.heads[f.headIdx].speedIx);
    events[f.headIdx].push({
      at: step * TICKS + Math.round((f.offNum * TICKS) / f.offDen),
      len: Math.max(1, Math.round((f.dur * TICKS * rate.d) / rate.n)),
      note: f.pitch - 60,
    });
  }
}
const total = playBars * 16 * TICKS;
const voiceText = (evs) => {
  const out = []; let at = 0;
  evs.forEach((e, i) => {
    if (e.at > at) out.push(`~@${e.at - at}`);
    const end = Math.min(e.at + e.len, i + 1 < evs.length ? evs[i + 1].at : total, total);
    out.push(`${e.note}@${end - e.at}`);
    at = end;
  });
  if (at < total) out.push(`~@${total - at}`);
  return out.join(" ");
};

console.log(`-- Melichthys: seed ${seed}, a ${steps / 16}-bar line of ${line.length} notes, ${root === 0 ? "C" : "root " + root} ${scaleName}; ${playBars} bars played`);
console.log(`-- plan:  ${planText(melody)}`);
console.log(`-- motif: ${lineText(melody.motif)}`);
specs.forEach((s, i) => console.log(`-- head ${["I", "II", "III", "IV"][i]}: ×${rates[s.speedIx]} ${["fwd", "back", "pend"][s.direction]}${s.invert ? " inv" : ""}${s.degrees ? " " + (s.degrees > 0 ? "+" : "") + s.degrees : ""}, in at bar ${s.enterBars + 1}, ${events[i].length} notes`));
console.log();
// one string, the heads layered with commas: mini-notation both engines read
// (Architeuthis's line language has no lists, so no `stack [..]`)
const layers = events.map((evs) => `[${voiceText(evs)}]`).join(", ");
console.log(`d1 $ slow ${playBars} $ note "${layers}" # s "${sound}"`);
