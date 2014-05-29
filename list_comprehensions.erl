-module(list_comprehensions).

-export([sums_list/2, inverted_sums_list/2]).

sums_list(From, To) ->
    List = lists:seq(From, To),
    [{X, Y, X+Y} || X <- List, Y <- List].

inverted_sums_list(From, To) ->
    SumsList = sums_list(From, To),
    [{X + Y, X, Y} || {X, Y, _} <- SumsList].