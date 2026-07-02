// Native JS 32-bit xorshift32. `| 0` / `>>>` keep it in ECMAScript's ToInt32/
// ToUint32 world; the returned state is signed-32 but only its low bits are used.
export const xorshift32 = s0 => {
  let s = s0 | 0;
  s ^= s << 13;
  s ^= s >>> 17;
  s ^= s << 5;
  return s | 0;
};
