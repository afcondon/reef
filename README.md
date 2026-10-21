# reef

The engines of the Atlantis virtual modules, written once in PureScript and
compiled to both JavaScript and Erlang.

The same source runs in the browser, inside
[Triggerfish](https://github.com/afcondon/triggerfish), and on the BEAM, inside
[Architeuthis](https://github.com/afcondon/architeuthis). A conformance suite
holds the two runtimes to identical output, so a pattern sounds the same
wherever it is played.

## What is in it

- **The machines.** The sequencer cores, each a pure step function plus its
  wire protocol:
  - `Reef.Odonus`: a grid sequencer, René/Cartesian style, with Fugue-Machine
    multi-playheads.
  - `Reef.Balistes` and `Reef.Vetula`.
  - `Reef.Conspicillum.*`: a grain machine whose scenes are written as one
    `#`-separated line, as Tidal writes a pattern. It replaced the retired
    Stellatus. Its parameter catalogue is generated into
    `docs/CONSPICILLUM-REFERENCE.md`.
- **Selene.** `Reef.Selene.*` is the polysignal model: the LFOs, clocks,
  Euclids, envelopes, random sources and notes that es9-daemon and fh2-config
  realise on an ES-9 or an FH-2.
- **Shared pieces.** Scales, pitch sets, voices, routing, articulation and
  Euclidean rhythms (`Reef.Euclid`), plus `Reef.Numeric` and `Reef.Bits`, the
  FFI that gives both runtimes the same arithmetic.

Pitch content (chords, voicings, voice leading) comes from
[harmonia](https://github.com/afcondon/purescript-harmonia). reef never
imports Littorina (the Tidal engine). The hosts sample Tidal patterns
themselves and hand reef plain values, which keeps reef free of the GPL.

## Building and testing

```sh
spago build
spago test                        # unit tests, plus the JS goldens
bash conformance/cross-runtime.sh # node against Erlang, every golden
```

`cross-runtime.sh` builds the Erlang side through a sibling checkout of
Architeuthis (`../live-coding/architeuthis`). Each `Reef.Conformance` run must
print the same text on node and on the BEAM, matching its file in
`conformance/*-golden.txt`.

One trap to know: purerl's `Int` division truncates while JavaScript's floors.
So reef divides with `floorDivI` from `Reef.Odonus` wherever a negative
operand is possible.

## Licence

MIT. See `LICENSE`.
