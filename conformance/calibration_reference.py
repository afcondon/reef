#!/usr/bin/env python3
"""A second realiser, written apart from Reef.Calibration, replaying
conformance/vectors/calibration.json.

The rule, from the comments of the three copies and not from their code:
sort the points by volts; at or below the first point's pitch answer the
first voltage, at or above the last point's the last; otherwise find the
first neighbouring pair whose pitches bracket the target and interpolate
between their voltages, linearly in the LOGARITHM of frequency; if the pair
has one pitch twice, answer the lower point's voltage.

Exit 0 when every case agrees to the file's tolerance.
"""
import json, math, os, sys

path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "vectors", "calibration.json")
vectors = json.load(open(path))
assert vectors["calibration-vectors"] == 1, "a vector format this replayer understands"
assert vectors["cases"], "no cases: nothing would be tested"


def realise(points, hz):
    points = sorted(points, key=lambda point: point["volts"])
    if hz <= points[0]["hz"]:
        return points[0]["volts"]
    if hz >= points[-1]["hz"]:
        return points[-1]["volts"]
    for low, high in zip(points, points[1:]):
        if low["hz"] <= hz <= high["hz"]:
            if low["hz"] == high["hz"]:
                return low["volts"]
            share = math.log(hz / low["hz"]) / math.log(high["hz"] / low["hz"])
            return low["volts"] + share * (high["volts"] - low["volts"])
    return points[-1]["volts"]


failures = 0
for case in vectors["cases"]:
    got = realise(vectors["tables"][case["table"]], case["hz"])
    if abs(got - case["volts"]) > vectors["toleranceVolts"]:
        failures += 1
        print(f"FAIL  {case['name']}: {got!r}, the vector file says {case['volts']!r}")
print(f"calibration: {len(vectors['cases'])} cases, {failures} failures")
sys.exit(1 if failures else 0)
