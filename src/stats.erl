-module(stats).

-export([minimum/1, maximum/1, range/1, mean/1, stdv/1]).

minimum([H|T]) ->
    minimum(T, H).

minimum([], Min) -> Min;
minimum([H|T], Min) ->
    minimum(T, min(H, Min)).

maximum([H|T]) -> maximum(T, H).

maximum([], Max) -> Max;
maximum([H|T], Max) ->
    maximum(T, max(H, Max)).

range(List) -> [minimum(List), maximum(List)].

mean(List) ->
    sum(List) / length(List).

stdv(List) ->
    N = length(List),
    {Sum, SumOfSquares} = lists:foldl(fun(X, {Sum, SumOfSquares}) -> {Sum + X, SumOfSquares + X * X} end, {0, 0}, List),
    math:sqrt((N * SumOfSquares - Sum * Sum) / (N * (N - 1))).

sum(List) ->
    lists:foldl(fun(X, Sum) -> X + Sum end, 0, List).
