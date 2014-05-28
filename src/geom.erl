-module(geom).

-export([area/1, area_case/3]).

%% @doc Calculates the area of a rectangle
-spec(area({atom(), number(), number()}) -> number()).

area({Shape, A, B}) ->
    area(Shape, A, B).

area(rectangle, A, B) when A >= 0, B >= 0 ->
    A * B;
area(ellipse, A, B) when A >= 0, B >= 0 ->
    math:pi() * A * B;
area(triangle, A, B) when A >= 0, B >= 0 ->
    (A * B) / 2;
area(_, _, _) -> 0.

area_case(Shape, A, B) when A >= 0, B >= 0 ->
    case Shape of
        rectangle -> A * B;
        ellipse -> math:pi() * A * B;
        triangle -> (A * B) / 2
    end.

