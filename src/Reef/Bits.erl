-module(reef_bits@foreign).

-export([xorshift32/1]).

%% xorshift32 keeping state masked to unsigned 32-bit so the result matches
%% ECMAScript's ToInt32/ToUint32 semantics bit-for-bit. JS `s << 13` truncates to
%% 32 bits (masked bsl); JS `s >>> 17` is a logical shift of the unsigned value
%% (bsr on a masked, hence non-negative, integer); `^` is bitwise (bxor). The low
%% byte the caller extracts is identical whether the state is read as JS-signed or
%% BEAM-unsigned, so no signed round-trip is needed.
xorshift32(S0) ->
    W = 16#FFFFFFFF,
    S = S0 band W,
    S1 = (S bxor ((S bsl 13) band W)) band W,
    S2 = S1 bxor (S1 bsr 17),
    S3 = (S2 bxor ((S2 bsl 5) band W)) band W,
    S3.
