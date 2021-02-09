
-module mobius.
-export [is_prime/1, prime_factors/1, is_square_multiple/1, find_square_multiples/2].
% -export [test/0].

is_prime(1) -> true;
is_prime(N) ->
	End = trunc(math:sqrt(N)),
	is_prime(N, 2, End).

is_prime(_, C, End) when C > End -> true;
is_prime(N, C, End) ->
	if
		N rem C == 0 -> false;
		true -> is_prime(N, C + 1, End)
	end.

prime_factors(N) ->
	prime_factors(N, 2, []).

prime_factors(N, C, Result) when C > N -> Result;
prime_factors(N, C, Result) ->
	IsPrime = is_prime(C),
	if 
		IsPrime ->
			if
				N rem C == 0 -> prime_factors(N div C, C, [C | Result]);
				true -> prime_factors(N, C + 1, Result) % is this required?
			end;
		true -> prime_factors(N, C + 1, Result)
	end.

is_square_multiple(N) -> 
	PrimeFactors = prime_factors(N),
	is_square_multiple(N, PrimeFactors).

is_square_multiple(_, [_]) -> false;
is_square_multiple(N, [V0 | Rest]) ->
	V1 = lists:nth(1, Rest),
	if 
		V0 == V1 -> true;
		true -> is_square_multiple(N, Rest)
	end.

find_square_multiples(Count, MaxN) ->
	find_square_multiples(Count, 2, 0, MaxN).

find_square_multiples(_, C, _, MaxN) when C >= MaxN -> fail;
find_square_multiples(Count, C, Curr, MaxN) ->
	% io:format("~p~n", [C]),
	IsSquareMultiple = is_square_multiple(C),
	if
		IsSquareMultiple -> 
			if
				Curr == Count - 1 -> C + 1 - Count;
				true -> find_square_multiples(Count, C + 1, Curr + 1, MaxN)
			end;
		true -> find_square_multiples(Count, C + 1, 0, MaxN)
	end.
