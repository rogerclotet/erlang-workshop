-module(dates).

-export([date_parts/1]).

%% Takes a string with format yyyy-mm-dd and returns a list with [yyyy, mm, dd]
date_parts(Str) ->
    [Year, Month, Day] = re:split(Str, "-", [{return, list}]),
    validate_date([
        element(1, string:to_integer(Year)),
        element(1, string:to_integer(Month)),
        element(1, string:to_integer(Day))
    ]).

validate_date([Year, Month, Day]) when Year > 0, Month > 0, Month =< 12, Day > 0, Day =< 31 ->
    [Year, Month, Day];
validate_date(_) ->
    {error, wrong_format}.