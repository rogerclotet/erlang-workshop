-module(classify).
-export([males_over_40/1, males_or_over_40/1]).

males_over_40(List) ->
    [Name || {Name, $M, Age} <- List, Age >= 40].

males_or_over_40(List) ->
    [Name || {Name, Gender, Age} <- List, Gender =:= $M orelse Age >= 40].
