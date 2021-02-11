
-module proc_sieve.
-export [generate/1, sieve/0, sieve/1].

		
generate(MaxN) ->
	S = lists:seq(2, MaxN),
	SievePid = spawn(proc_sieve, sieve, []),
	lists:foreach(
		fun(N) ->
			SievePid ! N
		end, S),
		SievePid ! {done, self()},
	receive
		{back, Result} -> 
			lists:foreach(
				fun(N) ->
					io:format("~p~n", [N])
				end, Result)
	end.

sieve() -> 
	sieve({undefined, undefined, undefined}).
sieve(State = {N, NextPid, PrevPid}) ->
	if
		N == undefined ->
			receive
				P -> sieve({P, NextPid, PrevPid})
			end;
		true ->
			receive
				{back, L} ->
					PrevPid ! {back, [N] ++ L},
					ok;
				{done, ReqPid} ->
					if
						NextPid == undefined ->
							ReqPid ! {back, [N]},
							ok;
						true ->
							NextPid ! {done, self()},
							sieve({N, NextPid, ReqPid})
					end;
				P ->
					if
						P rem N == 0 ->
							sieve(State);
						true ->
							if 
								NextPid == undefined ->
									NewNextPid = spawn(proc_sieve, sieve, []),
									NewNextPid ! P,
									sieve({N, NewNextPid, PrevPid});
								true ->
									NextPid ! P,
									sieve(State)
							end
					end
			end
	end.
