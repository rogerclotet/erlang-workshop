-module(ask_area).

-export([area/0]).

area() ->
    Input = io:get_line("R)ectangle, T)riangle, or E)llipse > "),
    Shape = char_to_shape(hd(Input)),
    D1 = to_number(io:get_line("First dimension  > ")),
    D2 = to_number(io:get_line("Second dimension > ")),
    io:format("~w~n", [{Shape, D1, D2}]),
    geom:area({Shape, D1, D2}).

char_to_shape(Char) ->
    case Char of
        $R -> rectangle;
        $r -> rectangle;
        $T -> triangle;
        $t -> triangle;
        $E -> ellipse;
        $e -> ellipse;
        _ -> unknown
    end.

to_number(String) ->
    {Test, _} = string:to_float(String),
    case Test of
        error -> {N, _} = string:to_integer(String);
        _ -> N = Test
    end,
    N.