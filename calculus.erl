-module(calculus).
-export([derivative/2]).

-define(DELTA, 1.0e-10).

derivative(F, X) ->
    (F(X + ?DELTA) - F(X)) / ?DELTA.
