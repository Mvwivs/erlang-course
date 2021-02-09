
-module fib.
-export [fib_p/1, fib_g/1, tail_fib/1].
-export [test/0].

fib_p(0) -> 0;
fib_p(1) -> 1;
fib_p(N) -> fib_p(N - 1) + fib_p(N - 2).

fib_g(N) when N > 1 ->
	fib_g(N - 1) + fib_g(N - 2);
fib_g(0) -> 0;
fib_g(1) -> 1.

tail_fib(N) -> fib_helper(N, 1, 0).
fib_helper(0, _, Curr) -> Curr;
fib_helper(N, Prev, Curr) -> 
	fib_helper(N - 1, Curr, Prev + Curr).


test_fib(F) ->
	0 = F(0),
	1 = F(1),
	1 = F(2),
	2 = F(3),
	55 = F(10),
	ok.

test() ->
	test_fib(fun fib_p/1),
	test_fib(fun fib_g/1),
	test_fib(fun tail_fib/1).
	