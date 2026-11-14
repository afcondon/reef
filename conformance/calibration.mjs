// Writes conformance/vectors/calibration.json from Test.CalibrationVectors
// (`write`), or fails if the file is not what the reference gives (`check`).
// After `spago build` (the test sources: `spago test` builds them).
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
const here = (path) => fileURLToPath(new URL(path, import.meta.url));
const { vectors } = await import(here("../output/Test.CalibrationVectors/index.js"));
const path = here("vectors/calibration.json");
JSON.parse(vectors); // it must at least be JSON
if (process.argv[2] === "write") writeFileSync(path, vectors + "\n");
else if (readFileSync(path, "utf8") !== vectors + "\n") {
  console.log("FAIL  conformance/vectors/calibration.json is not what the reference gives; run `node conformance/calibration.mjs write`");
  process.exit(1);
} else console.log("calibration vectors: current");
