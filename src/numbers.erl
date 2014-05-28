-module(numbers).
-export([abs/1]).

abs(N) when N < 0 -> -N;
abs(N) -> N.

