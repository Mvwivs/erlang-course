
- module (rss_reader).
- export [start/2, server/2].
- include ("logging.hrl").

start(Url, QPid) ->
	spawn(rss_reader, server, [Url, QPid]).

- define (RETRIEVE_INTERVAL, 600000).

server(Url, QPid) ->
	Result = case make_request(Url) of 
		{ok, Data} -> 
			Xml = xmerl_scan:string(Data),
			case rss_parse:is_rss2_feed(Xml) of
				true -> 
					{Rss, _} = Xml,
					rss_queue:add_feed(QPid, Rss),
					?INFO("Done reading rss: ~s~n", [Url]),
					ok;
				false -> {error, "Not RSS 2"}
			end;
		{error, Info} -> {error, Info}
	end,
	case Result of
		ok -> ok;
		{error, Reason} -> 
			?INFO("Error retrieving url: ~p~n", [Reason]),
			exit(Reason)
	end,
	receive after ?RETRIEVE_INTERVAL -> ok end,
	server(Url, QPid).

make_request(Url) ->
	Result = httpc:request(Url),
	case Result of
		{error, Reason} -> {error, Reason};
		{ok, Data} -> 
			{Status, _Headers, Body} = Data,
			{_Proto, Code, Text} = Status,
			case Code of
				200 -> {ok, Body};
				_Else -> {error, Text}
			end
	end.
