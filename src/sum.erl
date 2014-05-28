% Implement a function that sums a list of numbers

-module(sum).

-export([sum/1, range/1, test_range/0]).

sum([]) -> 0;
sum([Head | Tail]) ->
    Head + sum(Tail).

range(Num) ->
    range_list(Num, []).

range_list(0, List) ->
    List;
range_list(Num, List) ->
    range_list(Num - 1, [Num] ++ List).

test_range() ->
    Seq = lists:seq(1, 1000),
    Seq = range(1000),
    ok.
