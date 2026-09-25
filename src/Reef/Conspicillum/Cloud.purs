-- | `Reef.Conspicillum.Cloud` — the cloud over one cycle: where grains fall,
-- | and what happens to each one.
-- |
-- | C2 answered *which sample*. This answers *when, and then what* — and it is
-- | the step where the instrument stops being a granular module.
-- |
-- | ## A cloud is a pattern, so a grain has an ordinal
-- |
-- | Arbhar's grain stream is one anonymous continuous process: you set density
-- | and spray and it rains, and there is no way to say anything about *one*
-- | grain, because no grain has a name. Here density is a list of onsets — the
-- | mini-notation the player typed, already resolved — so grain 7 is a thing
-- | that exists and can be spoken about. `Every 3 0` is `every 3` over grains,
-- | and it costs one Int.
-- |
-- | That single fact is the whole difference, and everything else here follows
-- | from it.
-- |
-- | ## Why onsets and not mini-notation
-- |
-- | The parser lives in purerl-tidal, which depends on reef and not the other
-- | way round, and pulling it down here to chase a string would be the tail
-- | wagging the dog. The house pattern is the one Stellatus set: the frontend
-- | resolves its text to a fully-determined scene and pushes it ONCE
-- | (`Triggerfish.Tidal.Lane.onsetsOf` already returns exactly these fractional
-- | times). Reef consumes the resolved form. A cloud is then a pure function of
-- | its scene, which is what lets the browser draw it and the BEAM sound it
-- | from the same definition.
-- |
-- | ## Why this is cycle-addressed, where Stellatus loops
-- |
-- | `Reef.Stellatus.Engine.walk` precomputes a finite walk and repeats it,
-- | threading the seed left to right. That is right for a ring of a dozen
-- | slots and wrong here, for two reasons.
-- |
-- | A cloud of hundreds of grains a cycle would need a very long precomputed
-- | array before the repeat stopped being audible — and a cloud that repeats
-- | exactly is heard as a loop in a way a drum pattern is not.
-- |
-- | More importantly, Conspicillum's browser side is a pure visualizer: it
-- | recomputes what the rig is sounding rather than being told. Recomputing
-- | means being able to ask for cycle 400 *directly*, without simulating the
-- | 399 before it. So the seed for a cycle is derived from the base seed and
-- | the cycle number rather than threaded — which is also Tidal's own model,
-- | where a pattern is a function from a time arc to the events in it.
module Reef.Conspicillum.Cloud
  ( When(..)
  , Kind(..)
  , Step(..)
  , Op(..)
  , Rule
  , rule
  , Fx
  , Chain
  , noFx
  , noChain
  , Walk
  , noWalk
  , Swing
  , noSwing
  , Tape
  , oneBar
  , Steps
  , noSteps
  , swingWarp
  , Send
  , Spec
  , Emit
  , cycleSeed
  , applies
  , cycleOf
  ) where

import Prelude

import Data.Array (concatMap, foldl, index, length, mapWithIndex, modifyAt, range, replicate, snoc)
import Data.Int (floor, round, toNumber)
import Data.Int.Bits (and)
import Data.Maybe (Maybe(..), fromMaybe)
import Reef.Bits (xorshift32)
import Reef.Conspicillum.Corpus (Cloud, Corpus, Grainable, Query, clampTo, grainAt, pick, wrap01)
import Reef.Marbles (Seed, nextRand, seedFrom)

-- ── when a rule applies ──────────────────────────────────────────────────────

-- | Two ways to select grains, and they are different in kind.
-- |
-- | `Every n k` is **positional and certain**: grain ordinals `k, k+n, k+2n…`,
-- | counted across cycles so the figure does not restart every bar. This is the
-- | one no hardware granulator has, because it needs a grain to have an
-- | identity.
-- |
-- | `Chance p` is **stochastic and seeded**: reproducible from the scene, which
-- | the Link-less granulators cannot offer either — their randomness is gone
-- | the moment it happens.
data When
  = Always
  | Every Int Int
  | Chance Number
  -- | Tidal's `fix`: select a grain by what it READS, not when it falls. The
  -- | grain fires if the slice under its read head scores at least the
  -- | threshold for this kind (`Corpus.Grainable.hits`). Read after the walk,
  -- | before the rules, so a jumped-to snare is still a snare; a sample with
  -- | no scores never fires. The threshold is the reliability dial: high is
  -- | "certainly a snare", low lets the clap and the kick's click in.
  | Hit Kind Number

derive instance eqWhen :: Eq When

-- | What a slice can sound like, as far as the scores go.
data Kind = Kick | Snare | Hat

derive instance eqKind :: Eq Kind

-- | How a rule's `values` advance — Tidal's two ways of spreading a list over
-- | time. `PerHit` is `"a b c"`: one value per firing, restarting each cycle.
-- | `PerBar` is `"<a b c>"`: one value per cycle, whatever fires in it.
data Step = PerHit | PerBar

derive instance eqStep :: Eq Step

-- | What a rule does to a grain. Multiplicative where multiplication is the
-- | musical operation (`speed`, `gain`, grain length), absolute where it is not
-- | (`pan`, `accelerate` — the latter being a ratio already).
-- |
-- | **Constructors 5 up are the per-grain effects, and they are all absolute.**
-- | Multiplying into an effect that is off would leave it off, so these read
-- | "engage this effect on the grains this rule selects" — which is the whole
-- | figure. `Every 5 0 → OpCrush 4.0` is a bitcrushed grain every fifth, and it
-- | is the same argument `Every 3 0 → OpSpeed (-1.0)` makes: no hardware
-- | granulator can express it, because there a grain has no identity to count.
-- |
-- | Append-only, and enforced by the wire numbering in
-- | `Reef.Conspicillum.Protocol`. Add at the end; never reorder.
data Op
  = OpSpeed Number
  | OpGain Number
  | OpLength Number
  | OpPan Number
  | OpAccelerate Number
  | OpShape Number
  | OpCrush Number
  | OpCoarse Number
  | OpLpf Number
  | OpHpf Number
  | OpBpf Number
  | OpRes Number
  | OpVowel Number
  | OpPshift Number
  | OpTremolo Number
  | OpPhaser Number
  | OpGenv Number
  | OpGtilt Number
  | OpGplat Number
  | OpAtk Number
  | OpHold Number
  | OpRel Number
  | OpCurve Number
  | OpRsnPitch Number
  | OpRsnDecay Number
  | OpRsnBright Number
  | OpRsnMix Number
  | OpRsnModel Number
  -- | Move this grain's read head by a fraction of the source, wrapping. On a
  -- | following cloud (`Cloud.follow`) that is a jump in the tape: `-1/16` on
  -- | one sixteenth repeats the one before it, `+0.25` plays a beat ahead.
  -- | Additive, not multiplicative, because a position has no zero to scale.
  | OpShift Number
  -- | One grain becomes `n` inside its own slot, each `1/n` as long and every
  -- | one reading the grain's start: the stutter, the ratchet, the roll. Rules
  -- | that ran before it shape every repeat alike; the repeats are not grains
  -- | of their own, so they have no ordinals and no rule can pick them out.
  | OpRatchet Number
  -- | Send this grain to send `n` (1-based) of `Spec.sends`: the dub
  -- | producer's flick of a send knob. The grain still plays where it was, and
  -- | a COPY at the send's level plays through the send's chain — its own
  -- | orbit, which SuperDirt can put on its own output pair (3/4, 5/6) for an
  -- | Ableton return with whatever reverb or delay is on it. 0, or a send that
  -- | does not exist, sends nothing.
  | OpSend Number

-- | `values`, when not empty, replaces the op's own amount with a sequence
-- | (a control pattern, in Tidal's terms): kicks through a resonator with
-- | `values [36, 36, 39, 31]` stepping `PerBar` is a bassline played by the
-- | kick drum. Empty means the op's amount, as rules always were.
type Rule = { when :: When, op :: Op, values :: Array Number, step :: Step }

-- | A rule with no sequence: the op's own amount every time.
rule :: When -> Op -> Rule
rule when op = { when, op, values: [], step: PerHit }

-- ── effects ──────────────────────────────────────────────────────────────────

-- | The per-event SuperDirt effects — the ones that are genuinely **per grain**.
-- |
-- | ## Zero is off, everywhere, and that is load-bearing
-- |
-- | Every per-event module in SuperDirt's `core-modules.scd` is gated on its
-- | parameter being **present in the event** (`{ ~cutoff.notNil }` and so on),
-- | not on its value. So there is no neutral number to send: an `lpf` of 0 is a
-- | filter at 0 Hz, which is silence, not "no filter". The encoder must OMIT
-- | the key, and it needs a way to know when.
-- |
-- | Zero is that way, for all thirteen. It is out of range for every one of
-- | them as a musical value — a cutoff, a rate, a bit depth and a pitch ratio
-- | are all meaningless at zero — so it can carry "absent" without ever
-- | colliding with something a player might mean. A `noFx` record is then
-- | exactly today's behaviour: nothing sent, no module instantiated, no CPU.
-- |
-- | The numbers themselves are SuperDirt's, not ours, so that a value learnt
-- | here transfers to a Tidal pattern unchanged:
-- |
-- | - `shape` 0..~0.99, distortion; above 0.99 it is clipped by the synthdef
-- | - `crush` bit depth — 4 is brutal, 16 is transparent
-- | - `coarse` sample-rate decimation; the module itself ignores anything ≤ 1
-- | - `lpf` / `hpf` / `bpf` cutoff or centre in Hz (`cutoff` / `hcutoff` /
-- |   `bandf` on the wire)
-- | - `res` resonance, shared by `lpf`, `hpf` and `vowel` — one knob, because
-- |   three would be three knobs nobody turns
-- | - `vowel` 1..5 for a, e, i, o, u; an index rather than a string so a rule's
-- |   payload stays one Number
-- | - `pshift` a pitch **ratio**, 1.0 being unity. The one that earns its place:
-- |   it transposes independently of `speed`, so a grain moves in pitch without
-- |   moving in time, which a tape-speed read head structurally cannot do
-- | - `tremolo` / `phaser` rate in Hz, with their own depths
type Fx =
  { shape :: Number
  , crush :: Number
  , coarse :: Number
  , lpf :: Number
  , hpf :: Number
  , bpf :: Number
  , res :: Number
  , vowel :: Number
  , pshift :: Number
  , tremolo :: Number
  , tremdepth :: Number
  , phaser :: Number
  , phdepth :: Number
  -- | The grain's own amplitude envelope, which is a DIFFERENT thing from its
  -- | window: the window says which audio the grain is cut from, the envelope
  -- | says how that audio arrives and leaves. `genv` engages SuperDirt's
  -- | `grenvelo` (`gtilt` is where the peak sits, so 0.9 is a reverse swell;
  -- | `gplat` is the plateau, so a grain can be a burst rather than a bump).
  -- | `atk`/`rel` engage the plain ASR instead, and `curve` is shared — 0 is
  -- | linear, and it is sent whenever either envelope runs rather than gated
  -- | on its own value, because 0 is meaningful for it.
  , genv :: Number
  , gtilt :: Number
  , gplat :: Number
  , atk :: Number
  , hold :: Number
  , rel :: Number
  , curve :: Number
  -- | The resonator — OURS, not SuperDirt's. See `superdirt-daemon.scd`, where
  -- | a SynthDef and one `addModule` put it on the same footing as `crush`.
  -- |
  -- | This is the only effect here that does not colour the grain: it makes the
  -- | grain the EXCITER of something else, so the pitch comes from `rsnpitch`
  -- | and the timbre from the recording. `rsnpitch` is a MIDI note, so 0 is
  -- | 8.2 Hz and safely unusable — zero-is-off holds here as everywhere.
  -- | `rsnbright` is how much of its decay each successive partial keeps, which
  -- | is the property that makes something sound struck. `rsnmodel` picks a
  -- | ringing bank (0) or Karplus-Strong (1).
  -- |
  -- | **Its tail is bounded by the grain.** SuperDirt's `dirt_gate` carries
  -- | `doneAction: 14` — free the surrounding group and everything in it —
  -- | after `sustain`, and a per-event synth cannot outlive that. So `rsndecay`
  -- | 2.5 on a 90 ms grain gives 90 ms of ring and sounds like a filtered
  -- | click; nothing errors, it simply does not do what was asked. Use it by
  -- | opening the gate wide and letting the ENVELOPE make the grain short —
  -- | a long `sustain` with `genv` on and `gtilt` near 0 is a percussive burst
  -- | of sample followed by the resonator singing out the window, which is how
  -- | a struck instrument behaves. `Chain.grsn` is the one that rings freely.
  , rsnpitch :: Number
  , rsndecay :: Number
  , rsnbright :: Number
  , rsnmix :: Number
  , rsnmodel :: Number
  }

-- | No effect engaged — and so, byte for byte on the wire, the cloud this
-- | engine played before any of them existed.
noFx :: Fx
noFx =
  { shape: 0.0, crush: 0.0, coarse: 0.0
  , lpf: 0.0, hpf: 0.0, bpf: 0.0, res: 0.0
  , vowel: 0.0, pshift: 0.0
  , tremolo: 0.0, tremdepth: 0.5, phaser: 0.0, phdepth: 0.5
  , genv: 0.0, gtilt: 0.25, gplat: 0.3, atk: 0.0, hold: 0.0, rel: 0.0, curve: 0.0
  , rsnpitch: 0.0, rsndecay: 1.5, rsnbright: 0.7, rsnmix: 1.0, rsnmodel: 0.0
  }

-- | The **per-orbit** effects: one reverb, one delay, one leslie for the whole
-- | chain, not one per grain. A cloud has one of these, not a rule about it.
-- |
-- | Zero does NOT mean absent here, and the asymmetry with `Fx` is real rather
-- | than an oversight. SuperDirt's `GlobalDirtEffect` keeps state across events
-- | and only ever resumes — so a reverb that has once been told `room 0.6`
-- | keeps reverberating until something says `room 0`. Stopping sending is not
-- | how you turn it off; sending zero is. So these ride on **every** grain,
-- | which costs nothing: the effect diffs against its own state and emits OSC
-- | only when a value actually changed.
-- |
-- | `orbit` names which chain all of this configures. It is the scene's orbit,
-- | not a per-grain choice — a rule that moved single grains between orbits
-- | would drag these settings with them and configure both chains identically,
-- | which is the opposite of the point. Per-grain routing exists instead as
-- | `Spec.sends` + `OpSend`: the grain moves to a chain of its own. Twelve orbits exist and the rest of the
-- | rig wants them; see CONSPICILLUM-DESIGN.md.
type Chain =
  { orbit :: Int
  , room :: Number
  , size :: Number
  , dry :: Number
  , delay :: Number
  , delaytime :: Number
  , delayfeedback :: Number
  , lock :: Number
  , leslie :: Number
  , lrate :: Number
  , lsize :: Number
  -- | The resonator that can actually ring. `Fx.rsnpitch` is per grain and its
  -- | tail is bounded by the grain — SuperDirt's `dirt_gate` frees the whole
  -- | event group after `sustain`, so a 2.5 s decay on a 90 ms grain is 90 ms
  -- | of decay. This one lives for the life of the orbit, so the whole cloud
  -- | excites ONE body: many grains striking one string. `grsn` is the send
  -- | amount and behaves like `room`.
  , grsn :: Number
  , grsnpitch :: Number
  , grsndecay :: Number
  , grsnbright :: Number
  }

-- | A dry chain. `size` and the two leslie rates keep SuperDirt's own defaults
-- | rather than 0, because they are shapes rather than amounts: the amount
-- | knobs (`room`, `delay`, `leslie`) are what hold this silent.
noChain :: Chain
noChain =
  { orbit: 0
  , room: 0.0, size: 0.4, dry: 0.0
  , delay: 0.0, delaytime: 0.25, delayfeedback: 0.4, lock: 0.0
  , leslie: 0.0, lrate: 6.7, lsize: 0.3
  , grsn: 0.0, grsnpitch: 45.0, grsndecay: 3.0, grsnbright: 0.85
  }

-- | Everything about the cloud that is not the corpus or the query.
-- |
-- | `onsets` are fractional positions in [0,1) over one cycle — the resolved
-- | density pattern. An empty list is a silent cloud, which is a legitimate
-- | thing to ask for and must not be confused with a filter that admitted
-- | nothing (see `cycleOf`).
-- | The playhead's own life inside one cycle, which is what makes a following
-- | cloud (`Cloud.follow`) a Sector rather than a slicer.
-- |
-- | `OpShift` is **displacement**: one grain looks elsewhere and the next is
-- | back on its own sixteenth. The walk is **relocation**: the head moves and
-- | carries on reading from where it landed, so every later grain in the bar
-- | inherits the jump and the break re-lays itself in a new order instead of
-- | stuttering over a fixed one. That is state, and it is kept honest the way
-- | the design brief proposed: **it resets on every cycle**. Each bar starts
-- | on the tape, walks, and comes home at the bar line — a fill, not a drift —
-- | and `cycleOf` stays a pure function of the scene and the cycle number.
-- |
-- | Per grain, one die decides, in this order of bands:
-- |
-- | - `home` — a displaced head returns to the tape (this grain reads its own
-- |   place again);
-- | - `hold` — the head does not advance: this grain reads what the last one
-- |   read, which is Sector's repeat, and a run of holds is a stutter that
-- |   the tape then catches up from;
-- | - `jump` — the head relocates to one of `grid` places in the cycle (16 =
-- |   any sixteenth, 4 = a beat), at most `reach` of them away from where it is
-- |   (0 = anywhere).
-- |
-- | The walk draws from its OWN seed stream, so a scene that does not walk is
-- | draw-for-draw the scene it was before walks existed. It moves the read head
-- | only; with `follow` at 0 there is no head to move, and it is inert.
type Walk =
  { jump :: Number
  , hold :: Number
  , home :: Number
  , grid :: Int
  , reach :: Int
  }

noWalk :: Walk
noWalk = { jump: 0.0, hold: 0.0, home: 0.0, grid: 16, reach: 0 }

-- | Swing, twice over: the swing the TAPE was played with, and the swing the
-- | grains are PLAYED with. Both are swing percentages as a fraction — where
-- | the off-beat falls inside its pair, 0.5 straight, 0.66 triplet, 0.75
-- | dotted — on a `grid` of 16ths (pairs within an 8th) or 8ths (pairs within
-- | a beat).
-- |
-- | The tape swing is where the slices are CUT: slice i starts on its own hit,
-- | late offbeats included. The play swing is where grains LAND. So a swung
-- | break re-ordered by the walk keeps its feel (an offbeat slice moved to a
-- | downbeat plays its hit ON the downbeat, not late of it), and a straight
-- | break played with `play` above 0.5 gains swing it never had. Equal values
-- | on a following cloud reproduce the tape exactly, swing and all.
-- |
-- | Swung slots and slices alternate long and short, so a grain lasts the
-- | shorter of its slot and its slice: where the slice is short it stops and
-- | leaves a sliver of silence rather than leaking the next hit's transient —
-- | the choice Ableton's Beats mode makes too.
type Swing = { tape :: Number, play :: Number, grid :: Int }

noSwing :: Swing
noSwing = { tape: 0.5, play: 0.5, grid: 16 }

-- | Straight time to swung time, cycle fractions in and out. Piecewise
-- | linear and monotone, so it can be applied after ratchets are laid out
-- | and their repeats still subdivide the swung slot evenly.
swingWarp :: Number -> Int -> Number -> Number
swingWarp m grid x =
  let pairs = toNumber (if grid < 2 then 1 else grid / 2)
      q = x * pairs
      p = toNumber (floor q)
      f = q - p
      f' = if f < 0.5 then f * 2.0 * m else m + (f - 0.5) * 2.0 * (1.0 - m)
  in (p + f') / pairs

-- | How much longer (or shorter) than straight the swung slot at `x` is.
swingStretch :: Number -> Int -> Number -> Number
swingStretch m grid x =
  let pairs = toNumber (if grid < 2 then 1 else grid / 2)
      q = x * pairs
      f = q - toNumber (floor q)
  in if f < 0.5 then 2.0 * m else 2.0 * (1.0 - m)

-- | How long the tape is, in cycles, and which of its bars each cycle plays.
-- |
-- | A one-bar break is `oneBar`. A two-bar chord progression is `bars 2`, and
-- | the head then reads bar `cyc mod 2` of it each cycle; `order` re-lays the
-- | bars — `[0, 1, 0, 3]` of a four-bar loop is a permutation of its phrase,
-- | the progression re-ordered — and is indexed by the cycle, so it stays
-- | addressable. Everything inside a cycle (the walk, swing, the step table)
-- | still happens within the one bar being read. Only a following cloud has
-- | bars; a scanning one reads the whole sample by `position` as it always did.
type Tape = { bars :: Int, order :: Array Int }

oneBar :: Tape
oneBar = { bars: 1, order: [] }

-- | Sector's per-step jump table. For step k of `grid` steps in the bar,
-- | `to !! k` is where the head relocates (-1: nowhere) with probability
-- | `p !! k` (missing: certain), and it carries on reading from there, as a
-- | walk jump does. All-certain targets are a fixed permutation of the bar —
-- | `[0, 1, 2, 3]` on a grid of 4 is the bar as it is, `[2, 3, 0, 1]` swaps
-- | its halves — and uncertain ones are the table Sector gave each step.
-- | Applied after the walk, so a table can move even the downbeat, which the
-- | random walk never does: that is a choice you wrote, not a die. Its draws
-- | come from their own stream, one per grain.
type Steps = { grid :: Int, to :: Array Int, p :: Array Number }

noSteps :: Steps
noSteps = { grid: 16, to: [], p: [] }

-- | One send bus. `level` scales the copy's gain, like a send knob.
type Send = { chain :: Chain, level :: Number }

type Spec =
  { onsets :: Array Number
  , cloud :: Cloud
  , walk :: Walk
  , swing :: Swing
  , tape :: Tape
  , steps :: Steps
  , rules :: Array Rule
  , speed :: Number
  , gain :: Number
  , pan :: Number
  , accelerate :: Number
  -- | The effects every grain starts with, before rules. `noFx` is the cloud
  -- | as it was before effects existed.
  , fx :: Fx
  -- | The one chain the whole cloud feeds. Not rule-addressable: see `Chain`.
  , chain :: Chain
  -- | Send buses: a chain on its own orbit and a send level, which `OpSend`
  -- | copies single grains to. This is how a per-grain effect that SuperDirt
  -- | only offers per orbit becomes addressable after all: not by moving a
  -- | grain's settings, which would reconfigure the orbit it lands on, but by
  -- | copying the grain to an orbit already set up for it. Usually that orbit
  -- | is dry and comes out on its own outputs, and the effect is in Ableton.
  , sends :: Array Send
  }

-- | One grain, placed in the cycle and fully resolved: everything
-- | `/dirt/play` needs and nothing it does not.
type Emit =
  { at :: Number
  , n :: Int
  , begin :: Number
  , end :: Number
  , sustain :: Number
  , speed :: Number
  , gain :: Number
  , pan :: Number
  , accelerate :: Number
  -- | This grain's effects, after its rules. The encoder drops every zero, so
  -- | a grain no effect rule selected costs exactly the OSC it always cost.
  , fx :: Fx
  -- | Carried unchanged from the spec. It is a property of the orbit rather
  -- | than of the grain, and it rides here only because the encoder sees one
  -- | grain at a time — see `Chain` for why it must be sent on every one.
  , chain :: Chain
  -- | How many times `OpRatchet` asked this grain to sound. `cycleOf` expands
  -- | it before anything leaves, so every Emit that reaches the rig says 1.
  , ratchet :: Int
  -- | Which send `OpSend` asked for; `cycleOf` turns it into a copy on the
  -- | send's chain, so every Emit that reaches the rig says 0.
  , send :: Int
  }

-- ── the per-cycle seed ───────────────────────────────────────────────────────

-- | The seed for one cycle, from the base seed and the cycle number.
-- |
-- | Built from `Reef.Bits.xorshift32`, the one bit primitive this package owns
-- | FFI for because it is proven bit-identical on V8 and the BEAM.
-- |
-- | **Only the low byte of a xorshift state may be used as a VALUE.** The state
-- | itself is signed-32 under JS and explicitly-masked unsigned-32 on the BEAM;
-- | the bits are the same, so chaining one step into the next agrees, but the
-- | *number* does not — and handing the raw state to something like `seedFrom`,
-- | which reduces modulo a constant, gives two different answers. That is not a
-- | hypothetical: this function did exactly that, `conspicillumCloudRun`
-- | diverged on every line while the C2 selector golden stayed byte-identical,
-- | and the two goldens together said "same selector, different seed" precisely
-- | enough to find it. `Reef.Balistes.Engine.randByte` is the idiom — chain the
-- | state, use `and 0xFF`.
-- |
-- | So three chained steps give three sign-agnostic bytes, assembled into 24
-- | bits. That is ~16.7M distinct cycle seeds, which at one cycle a couple of
-- | seconds is longer than any session.
-- |
-- | **Both inputs are reduced below 10^6 before they are added**, and that is
-- | not fussiness either: PureScript's `Int` is 32-bit under JS and
-- | arbitrary-precision under purerl, so an addition that overflows is another
-- | silent cross-runtime divergence. Nothing here can overflow on either.
-- |
-- | This is a mixer, not a hash. Its job is only that adjacent cycles should
-- | not sound related.
cycleSeed :: Int -> Int -> Seed
cycleSeed base n =
  let
    s1 = xorshift32 (small n + small base)
    s2 = xorshift32 s1
    s3 = xorshift32 s2
  in
    seedFrom ((s1 `and` 0xFF) + (s2 `and` 0xFF) * 256 + (s3 `and` 0xFF) * 65536)
  where
  -- Non-negative and bounded, whatever the caller passed and whichever
  -- runtime's `mod` sign convention applies.
  small :: Int -> Int
  small x = let m = x `mod` 1000003 in if m < 0 then m + 1000003 else m

-- ── rule selection ───────────────────────────────────────────────────────────

-- | Does this rule fire for the grain with this absolute ordinal?
-- |
-- | `Chance` draws, so it advances the seed; `Always` and `Every` do not.
-- |
-- | **A `Chance` rule draws unconditionally, before anything is decided.** The
-- | tempting optimisation is to skip the draw when an earlier rule already
-- | settled the grain — and it would make the draws of every later rule depend
-- | on how the earlier ones happened to land, so the cycle would stop being
-- | reproducible from its scene. Fixed draw ORDER is the whole guarantee, and
-- | it is easy to lose by being clever here.
applies :: Rule -> Int -> Scores -> Seed -> { fires :: Boolean, seed :: Seed }
applies r ordinal sc s = case r.when of
  Always -> { fires: true, seed: s }
  Every n k ->
    let period = if n < 1 then 1 else n
    in { fires: (ordinal - k) `mod` period == 0 && ordinal >= k, seed: s }
  Chance p ->
    let { u, seed } = nextRand s
    in { fires: u < p, seed }
  Hit k t ->
    let v = case k of
          Kick -> sc.kick
          Snare -> sc.snare
          Hat -> sc.hat
    in { fires: v >= t && v > 0.0, seed: s }

-- | The hit scores of the slice a grain reads.
type Scores = { kick :: Number, snare :: Number, hat :: Number }

scoresAt :: Grainable -> Number -> Scores
scoresAt g read =
  { kick: at g.hits.kick, snare: at g.hits.snare, hat: at g.hits.hat }
  where
  at xs =
    let n = length xs
        -- A hair past the slice start: a read placed exactly on a boundary
        -- must land in the slice it begins, not the one before, whichever
        -- way the division rounds.
        i = floor (wrap01 read * toNumber n + 1.0e-6)
    in if n == 0 then 0.0 else fromMaybe 0.0 (index xs (if i >= n then n - 1 else i))

-- | The op with its amount replaced — how a rule's `values` reach it.
setAmount :: Op -> Number -> Op
setAmount op x = case op of
  OpSpeed _ -> OpSpeed x
  OpGain _ -> OpGain x
  OpLength _ -> OpLength x
  OpPan _ -> OpPan x
  OpAccelerate _ -> OpAccelerate x
  OpShape _ -> OpShape x
  OpCrush _ -> OpCrush x
  OpCoarse _ -> OpCoarse x
  OpLpf _ -> OpLpf x
  OpHpf _ -> OpHpf x
  OpBpf _ -> OpBpf x
  OpRes _ -> OpRes x
  OpVowel _ -> OpVowel x
  OpPshift _ -> OpPshift x
  OpTremolo _ -> OpTremolo x
  OpPhaser _ -> OpPhaser x
  OpGenv _ -> OpGenv x
  OpGtilt _ -> OpGtilt x
  OpGplat _ -> OpGplat x
  OpAtk _ -> OpAtk x
  OpHold _ -> OpHold x
  OpRel _ -> OpRel x
  OpCurve _ -> OpCurve x
  OpRsnPitch _ -> OpRsnPitch x
  OpRsnDecay _ -> OpRsnDecay x
  OpRsnBright _ -> OpRsnBright x
  OpRsnMix _ -> OpRsnMix x
  OpRsnModel _ -> OpRsnModel x
  OpShift _ -> OpShift x
  OpRatchet _ -> OpRatchet x
  OpSend _ -> OpSend x

applyOp :: Op -> Emit -> Emit
applyOp op e = case op of
  OpSpeed x -> e { speed = e.speed * x }
  OpGain x -> e { gain = e.gain * x }
  -- The window scales with the grain, so a shorter grain reads less tape
  -- rather than the same tape faster: SuperDirt, and the voice's crossfade,
  -- both read the rate off window over sustain.
  OpLength x -> e { sustain = e.sustain * x, end = min 1.0 (e.begin + (e.end - e.begin) * x) }
  OpPan x -> e { pan = x }
  OpAccelerate x -> e { accelerate = x }
  -- Absolute, every one: see the note on `Op`. Setting a value engages the
  -- effect for this grain; setting 0 takes it off a grain the spec had it on.
  OpShape x -> e { fx = e.fx { shape = x } }
  OpCrush x -> e { fx = e.fx { crush = x } }
  OpCoarse x -> e { fx = e.fx { coarse = x } }
  OpLpf x -> e { fx = e.fx { lpf = x } }
  OpHpf x -> e { fx = e.fx { hpf = x } }
  OpBpf x -> e { fx = e.fx { bpf = x } }
  OpRes x -> e { fx = e.fx { res = x } }
  OpVowel x -> e { fx = e.fx { vowel = x } }
  OpPshift x -> e { fx = e.fx { pshift = x } }
  OpTremolo x -> e { fx = e.fx { tremolo = x } }
  OpPhaser x -> e { fx = e.fx { phaser = x } }
  OpGenv x -> e { fx = e.fx { genv = x } }
  OpGtilt x -> e { fx = e.fx { gtilt = x } }
  OpGplat x -> e { fx = e.fx { gplat = x } }
  OpAtk x -> e { fx = e.fx { atk = x } }
  OpHold x -> e { fx = e.fx { hold = x } }
  OpRel x -> e { fx = e.fx { rel = x } }
  OpCurve x -> e { fx = e.fx { curve = x } }
  OpRsnPitch x -> e { fx = e.fx { rsnpitch = x } }
  OpRsnDecay x -> e { fx = e.fx { rsndecay = x } }
  OpRsnBright x -> e { fx = e.fx { rsnbright = x } }
  OpRsnMix x -> e { fx = e.fx { rsnmix = x } }
  OpRsnModel x -> e { fx = e.fx { rsnmodel = x } }
  OpRatchet x -> e { ratchet = round x }
  -- Resolved in `cycleOf`, which knows the sends; see `routeSend`.
  OpSend _ -> e
  OpShift x ->
    let w = e.end - e.begin
        b = clampTo 0.0 (1.0 - w) (wrap01 (e.begin + x))
    in e { begin = b, end = b + w }

-- ── the cycle ────────────────────────────────────────────────────────────────

-- | Every grain of one cycle, fully resolved.
-- |
-- | A pure function of (corpus, query, spec, base seed, cycle number) — so any
-- | cycle can be asked for directly, and the browser and the rig computing the
-- | same one get the same answer.
-- |
-- | A grain whose query admits nothing is **dropped**, not defaulted to sample
-- | zero. The cloud then has fewer grains than it has onsets, which is exactly
-- | the truth and is a thing the surface should be able to show: "your filter
-- | excluded everything" and "you asked for a sparse cloud" must not look the
-- | same. Note that the seed still advances for a dropped grain, so a query
-- | narrowing does not reshuffle the grains that do survive.
cycleOf :: Corpus -> Query -> Spec -> Int -> Int -> Array Emit
cycleOf corpus query spec base cyc =
  map landSwung $ concatMap sendCopies $ concatMap expand
    (foldl step
       { seed: cycleSeed base cyc
       , head: { offset: 0.0, prevRead: 0.0, first: true, seed: walkSeed }
       , stepSeed: cycleSeed (base + 700001) cyc
       , fired: replicate (length spec.rules) 0
       , out: []
       }
       indexed).out
  where
  count = length spec.onsets

  -- Its own stream, so walking never moves a grain the walk did not touch.
  walkSeed = cycleSeed (base + 500009) cyc

  indexed :: Array { i :: Int, at :: Number }
  indexed = mapWithIndex (\i at -> { i, at }) spec.onsets

  -- The ordinal counts across cycles, so `Every 3 0` marks every third grain
  -- continuously rather than restarting each bar. At a few hundred grains a
  -- cycle this stays inside Int for something over two hundred days of
  -- continuous play, which is longer than the rig stays up.
  step acc o =
    let
      ordinal = cyc * count + o.i
      hd0 = walkStep spec.cloud.follow spec.walk acc.head o.at
      { u: us, seed: ss } = nextRand acc.stepSeed
      hd = stepJump spec.cloud.follow spec.steps hd0 o.at us
      read = spec.cloud.follow * o.at + hd.offset
      sw = spec.swing
      -- Read on the tape's own grid: slice i begins where its hit is.
      readSwung = if spec.cloud.follow == 0.0 then read else onTape (swingWarp sw.tape sw.grid read)
      fit = min (swingStretch sw.play sw.grid o.at) (swingStretch sw.tape sw.grid read)
      { chosen, seed: s1 } = pick query corpus acc.seed
    in case chosen of
      Nothing -> { seed: s1, head: hd, stepSeed: ss, fired: acc.fired, out: acc.out }
      Just g ->
        let
          -- Fitted BEFORE placing, so the window that must fit in the tape is
          -- the grain's real length: the last short slice of a swung bar sits
          -- nearer the end than a straight sixteenth would be allowed to.
          { grain, seed: s2 } = grainAt (spec.cloud { sustain = spec.cloud.sustain * fit }) readSwung g s1
          base' =
            { at: o.at
            , n: grain.n
            , begin: grain.begin
            , end: grain.end
            , sustain: grain.sustain
            , speed: spec.speed
            , gain: spec.gain
            , pan: spec.pan
            , accelerate: spec.accelerate
            , fx: spec.fx
            , chain: spec.chain
            , ratchet: 1
            , send: 0
            }
          sc = scoresAt g (if spec.cloud.follow == 0.0 then read else onTape read)
          r = foldl (applyRule ordinal sc) { e: base', seed: s2, fired: acc.fired, i: 0 } spec.rules
        in { seed: r.seed, head: hd, stepSeed: ss, fired: r.fired, out: snoc acc.out r.e }

  -- A ratcheted grain's slot runs to the next onset (round the bar for the
  -- last), and its repeats share it evenly.
  expand :: Emit -> Array Emit
  expand e
    | e.ratchet <= 1 = [ e { ratchet = 1 } ]
    | otherwise =
        let
          k = e.ratchet
          slot = slotOf e.at
          one j = e { at = e.at + slot * toNumber j / toNumber k
                    , sustain = e.sustain / toNumber k
                    , end = e.begin + (e.end - e.begin) / toNumber k
                    , ratchet = 1 }
        in map one (range 0 (k - 1))

  slotOf :: Number -> Number
  slotOf at =
    let later = foldl (\m x -> if x > at && x < m then x else m) (1.0 + first) spec.onsets
        first = case index spec.onsets 0 of
          Just x -> x
          Nothing -> 0.0
    in later - at

  -- `fired` counts each rule's firings in THIS cycle, which is what `PerHit`
  -- indexes by; it starts at zero every cycle, so the sequence restarts on
  -- the bar the way Tidal's does, and `cycleOf` stays cycle-addressable.
  applyRule ordinal sc acc r =
    let
      { fires, seed } = applies r ordinal sc acc.seed
      n = fromMaybe 0 (index acc.fired acc.i)
      op = case length r.values of
        0 -> r.op
        len ->
          let k = case r.step of
                PerHit -> n
                PerBar -> cyc
          in setAmount r.op (fromMaybe 0.0 (index r.values (posMod k len)))
    in
      { e: if fires then routeSend op (applyOp op acc.e) else acc.e
      , seed
      , fired: if fires then fromMaybe acc.fired (modifyAt acc.i (_ + 1) acc.fired) else acc.fired
      , i: acc.i + 1
      }

  -- The bar this cycle reads, and a within-bar read placed on the whole tape.
  bars = if spec.tape.bars < 1 then 1 else spec.tape.bars
  barIx = case length spec.tape.order of
    0 -> posMod cyc bars
    n -> posMod (fromMaybe 0 (index spec.tape.order (posMod cyc n))) bars
  onTape r = (toNumber barIx + wrapUnit r) / toNumber bars

  -- Onsets move last, after ratchet and sends, so everything that was laid
  -- out in straight time lands on the swung grid together.
  landSwung :: Emit -> Emit
  landSwung e = e { at = swingWarp spec.swing.play spec.swing.grid e.at }

  routeSend :: Op -> Emit -> Emit
  routeSend op e = case op of
    OpSend x -> e { send = round x }
    _ -> e

  -- The copy is made after ratchet, so a ratcheted grain sends every repeat.
  sendCopies :: Emit -> Array Emit
  sendCopies e =
    let dry = e { send = 0 }
    in case (if e.send >= 1 then index spec.sends (e.send - 1) else Nothing) of
      Nothing -> [ dry ]
      Just sd -> [ dry, dry { chain = sd.chain, gain = e.gain * sd.level } ]

  posMod :: Int -> Int -> Int
  posMod a m = let r = a `mod` m in if r < 0 then r + m else r

-- ── the walk ─────────────────────────────────────────────────────────────────

type Head = { offset :: Number, prevRead :: Number, first :: Boolean, seed :: Seed }

-- | Advance the playhead to the grain at `at`. Two draws per grain, always —
-- | the band die and the target die — so the stream never depends on which
-- | band fired. `prevRead` is where the last grain read, unwrapped; a hold
-- | reads it again.
walkStep :: Number -> Walk -> Head -> Number -> Head
walkStep follow w h at =
  let
    { u: u1, seed: s1 } = nextRand h.seed
    { u: u2, seed: s2 } = nextRand s1
    here = follow * at
    grid = if w.grid < 1 then 1 else w.grid
    gridF = toNumber grid
    cur = floor (wrapUnit (here + h.offset) * gridF)
    target =
      if w.reach <= 0 then floor (u2 * gridF)
      else
        let k = floor (u2 * toNumber (2 * w.reach))
            d = if k < w.reach then k - w.reach else k - w.reach + 1
        in cur + d
    offset
      -- The downbeat is always the tape's: a bar that could open anywhere
      -- would not come home, it would only change the subject.
      | h.first = 0.0
      | u1 < w.home = 0.0
      | u1 < w.home + w.hold = h.prevRead - here
      | u1 < w.home + w.hold + w.jump = toNumber target / gridF - here
      | otherwise = h.offset
  in { offset, prevRead: here + offset, first: false, seed: s2 }

wrapUnit :: Number -> Number
wrapUnit x = x - toNumber (floor x)

-- | Apply the step table at the grain that starts a step: relocate with the
-- | step's probability. See `Steps`.
stepJump :: Number -> Steps -> Head -> Number -> Number -> Head
stepJump follow st h at u =
  let
    grid = if st.grid < 1 then 1 else st.grid
    gridF = toNumber grid
    x = at * gridF
    k = round x
    starts = x - toNumber k < 1.0e-6 && toNumber k - x < 1.0e-6
    kk = let m = k `mod` grid in if m < 0 then m + grid else m
    p = fromMaybe 1.0 (index st.p kk)
  in case index st.to kk of
    Just t | starts && t >= 0 && u < p ->
      let r = toNumber t / gridF
      in h { offset = r - follow * at, prevRead = r }
    _ -> h
