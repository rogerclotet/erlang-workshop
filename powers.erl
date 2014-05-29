-module(powers).

-export([raise/2, raise_ntr/2, nth_root/2]).

-spec(raise(number(), number()) -> number()).

raise(X, N) ->
    raise(X, N, 1).

raise(_, 0, Acc) ->
    Acc;

raise(X, N, Acc) when N < 0 ->
    1.0 / raise(X, -N, Acc);

raise(X, N, Acc) when N > 0 ->
    raise(X, N - 1, Acc * X).

-spec(raise_ntr(number(), number()) -> number()).

raise_ntr(_, 0) ->
    1;

raise_ntr(X, 1) ->
    X;

raise_ntr(X, N) when N > 0 ->
    X * raise(X, N - 1);

raise_ntr(X, N) when N < 0 ->
    1.0 / raise(X, -N).

-spec(nth_root(number(), number()) -> number()).

nth_root(X, N) ->
    nth_root(X, N, X / 2.0).

nth_root(X, N, Approx) ->
    io:format("Current guess is ~w~n", [Approx]),

    F = raise(Approx, N) - X,
    Fprime = N * raise(Approx, N - 1),
    Next = Approx - F / Fprime,
    Change = abs(Next - Approx),

    if
        Change < 1.0e-8 ->
            Next;
        true ->
            nth_root(X, N, Next)
    end.

