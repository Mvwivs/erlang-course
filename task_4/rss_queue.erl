
- module rss_queue.
- export [start/0, server/0].
- export [add_item/2, add_feed/2, get_all/1].

server(State = {Queue}) ->
	receive 
		{add_item, RSSItem} -> 
			NewQueue = try_add_item(RSSItem, Queue),
			server({NewQueue});
		{get_all, ReqPid} -> 
			ReqPid ! {get_all, Queue},
			server(State)
	end.
server() ->
	server({[]}).

try_add_item(Item, Queue) ->
	CompRes = check_items(Item, Queue, 0),
	case CompRes of
		different -> 
			NewQueue = [Item | Queue],
			lists:sort(fun pub_date_comp/2, NewQueue);
		same -> Queue;
		{updated, Pos} -> 
			NewQueue = [Item | remove_from_list(Pos, Queue)],
			lists:sort(fun pub_date_comp/2, NewQueue)
	end.


check_items(_Item, [], _Pos) -> different;
check_items(Item, [Old | Rest], Pos) ->
	Res = rss_parse:compare_feed_items(Old, Item),
	case Res of
		updated -> {updated, Pos};
		same -> same;
		_Else -> check_items(Item, Rest, Pos + 1)
	end.

pub_date_comp(Item1, Item2) ->
	Time1 = rss_parse:get_item_time(Item1),
	Time2 = rss_parse:get_item_time(Item2),
	Time1 > Time2.

remove_from_list(Index, List) ->
	{Left, [_|Right]} = lists:split(Index - 1, List),
	Left ++ Right.

start() ->
	spawn(rss_queue, server, []).

add_item(QPid, Item) when is_pid(QPid) ->
	QPid ! {add_item, Item}.

add_feed(QPid, RSS2Feed) when is_pid(QPid) ->
	Items = rss_parse:get_feed_items(RSS2Feed),
	lists:foreach(fun(Item) -> 
		add_item(QPid, Item)
	end, Items),
	ok.

- define(TIMEOUT, 1000).

get_all(QPid) when is_pid(QPid) ->
	QPid ! {get_all, self()},
	receive
		{get_all, List} -> {ok, List}
	after ?TIMEOUT -> {error, timeout}
	end.
