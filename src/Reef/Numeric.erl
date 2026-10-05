-module(reef_numeric@foreign).

-export([pow/2, ln/1, sin/1]).

%% Mirrors purerl's math@foreign: a 2-arrow PureScript foreign is exported at
%% arity 2 and the compiler curries at the call site.
pow(X, Y) -> math:pow(X, Y).

ln(X) -> math:log(X).

sin(X) -> math:sin(X).
