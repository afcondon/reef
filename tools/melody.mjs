#!/usr/bin/env node
// Generate a melodic line (Reef.Melody) and print it for Limulus.
//
//   node tools/melody.mjs [--seed N] [--bars N] [--scale NAME] [--sound NAME]
//
// Prints the plan and the motif, then two Tidal blocks: the line alone, and
// a four-voice exposition (each voice a line later than the last: subject,
// answer a fifth up, subject an octave down, answer a fourth down).
// Run `spago build` in reef first.
import { generate, flatten, lineText, planText, lineSteps, transpose } from "../output/Reef.Melody/index.js";
import { seedFrom } from "../output/Reef.Marbles/index.js";

const arg = (name, fallback) => {
  const i = process.argv.indexOf("--" + name);
  return i > 0 ? process.argv[i + 1] : fallback;
};
const seed = parseInt(arg("seed", "1"), 10);
const bars = parseInt(arg("bars", "8"), 10);
const scale = arg("scale", "minor");
const sound = arg("sound", "superpiano");

const { melody } = generate(bars)(seedFrom(seed));
const line = flatten(melody);
const cycles = lineSteps(line) / 16;
const text = (by) => lineText(transpose(by)(line));

console.log(`-- seed ${seed}, ${cycles} bars, ${line.length} notes`);
console.log(`-- plan:  ${planText(melody)}`);
console.log(`-- motif: ${lineText(melody.motif)}`);
console.log();
console.log(`d1 $ slow ${cycles} $ n (scale "${scale}" "${text(0)}") # s "${sound}"`);
console.log();

// the exposition: voice v rests v lines, then plays to the end; 4 lines in all
const rest = (v) => (v === 0 ? "" : `~@${v} `);
const voice = (v, by) => `[${rest(v)}${Array(4 - v).fill(`[${text(by)}]`).join(" ")}]`;
// one string, the voices layered with commas: mini-notation both engines read
// (Architeuthis's line language has no lists, so no `stack [..]`)
const voices = [voice(0, 0), voice(1, 4), voice(2, -7), voice(3, -3)].join(", ");
console.log(`d1 $ slow ${cycles * 4} $ n (scale "${scale}" "${voices}") # s "${sound}"`);
