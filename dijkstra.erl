-module(dijkstra).

-export([gcd/2]).

-spec(gcd(number(), number()) -> number()).

gcd(M, N) when M =:= N -> 
    M;

gcd(M, N) when M > N -> 
    gcd(M - N, N);

gcd(M, N) -> 
    gcd(M, N - M).
