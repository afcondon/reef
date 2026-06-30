// reef-emit-node.mjs — the JS-runtime twin of reef_voice.erl.
//
// Drives the SAME Reef.Odonus engine (here the purs JS build, where
// reef_voice.erl drives the purerl Erlang build) and emits the fired notes
// over the SAME OSC path: /midi/note/at -> link-spike :57122 -> CoreMIDI ->
// IAC "Tidal" -> Ableton. Hardcoded to defaultOdonus, like reef_voice.
//
// The A/B: run this on one channel and `reef_voice:start(Ch, StepMs)` on
// another, same StepMs, started together. Same engine, two runtimes — you
// should hear unison (and the conformance golden's scale walk). Any drift is
// a real JS-vs-Erlang divergence, not a timing artifact (both schedule with
// the same +200ms lead through link-spike, so infra latency is shared).
//
//   node conformance/reef-emit-node.mjs [channel=15] [stepMs=150] [steps=Infinity]
//
// Needs the JS build present: `spago build` in the reef package first.

import dgram from "node:dgram";
import * as Odonus from "../output/Reef.Odonus/index.js";

const channel = Number(process.argv[2] ?? 15);
const stepMs = Number(process.argv[3] ?? 150);
const maxSteps = Number(process.argv[4] ?? Infinity);
const PORT_NAME = "IAC Driver Tidal";
const HOST = "127.0.0.1";
const LINK_SPIKE_PORT = 57122;
const LEAD_US = 200000n; // match reef_voice: schedule 200ms ahead

const sock = dgram.createSocket("udp4");

// OSC: pad a string to a 4-byte boundary with at least one trailing null.
function padString(s) {
  const raw = Buffer.from(s, "ascii");
  const len = raw.length + 1; // +1 for the mandatory null
  const pad = (4 - (len % 4)) % 4;
  return Buffer.concat([raw, Buffer.alloc(1 + pad)]); // null + padding (all zero)
}

// /midi/note/at  ,siiiih  port channel note velocity duration_ms unix_us_at
// Byte-identical to MIDIBridge.erl's encode_note_at.
function encodeNoteAt(portName, ch, note, vel, durMs, unixUsAt) {
  const addr = padString("/midi/note/at");
  const typeTag = padString(",siiiih");
  const port = padString(portName);
  const body = Buffer.alloc(4 * 4 + 8);
  body.writeInt32BE(Math.round(ch), 0);
  body.writeInt32BE(Math.round(note), 4);
  body.writeInt32BE(Math.round(vel), 8);
  body.writeInt32BE(Math.round(durMs), 12);
  body.writeBigInt64BE(BigInt(Math.round(Number(unixUsAt))), 16);
  return Buffer.concat([addr, typeTag, port, body]);
}

function nowUs() {
  return BigInt(Date.now()) * 1000n;
}

let odo = Odonus.defaultOdonus;
let n = 0;

console.log(`reef-emit-node: ch ${channel}, ${stepMs}ms/step, engine=JS, -> link-spike:${LINK_SPIKE_PORT}`);

const timer = setInterval(() => {
  if (n >= maxSteps) {
    clearInterval(timer);
    sock.close();
    return;
  }
  n += 1;
  const res = Odonus.stepEmit(odo);
  odo = res.odo;
  const fired = res.fired; // plain JS array under the JS backend
  const wallUs = nowUs() + LEAD_US;
  for (const f of fired) {
    const durMs = f.dur * stepMs;
    const pkt = encodeNoteAt(PORT_NAME, channel, f.pitch, f.vel, durMs, wallUs);
    sock.send(pkt, LINK_SPIKE_PORT, HOST);
  }
}, stepMs);

process.on("SIGINT", () => {
  clearInterval(timer);
  sock.close();
  console.log("\nreef-emit-node: stopped");
  process.exit(0);
});
