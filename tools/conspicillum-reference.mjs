// Write docs/CONSPICILLUM-REFERENCE.md from Reef.Conspicillum.Reference.
// Run from the reef root after `spago build`. The conformance suite compares
// the committed file with what node and the BEAM both print.
import { writeFileSync } from "node:fs";
const { reference } = await import(new URL("../output/Reef.Conspicillum.Reference/index.js", import.meta.url).href);
const out = new URL("../docs/CONSPICILLUM-REFERENCE.md", import.meta.url);
writeFileSync(out, reference);
console.log(`wrote ${reference.split("\n").length} lines to ${out.pathname}`);
