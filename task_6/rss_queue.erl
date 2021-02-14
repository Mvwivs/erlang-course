
- module (rss_queue).
- export [start/1, start/2, add_item/2, add_feed/2, get_all/1, subscribe/2, unsubscribe/1].
- export [try_add_item/2].
- export [test/0, preview_rss/1].
- include ("logging.hrl").

try_add_item(Item, Queue) ->
	CompRes = check_items(Item, Queue, 0),
	case CompRes of
		different -> 
			NewQueue = [Item | Queue],
			?INFO("Adding new item: ~p~n", [length(NewQueue)]),
			{updated, lists:sort(fun pub_date_comp/2, NewQueue)};
		same -> {same, Queue};
		{updated, Pos} -> 
			NewQueue = [Item | remove_from_list(Pos, Queue)],
			?INFO("Updating item: ~p~n", [length(NewQueue)]),
			{updated, lists:sort(fun pub_date_comp/2, NewQueue)}
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

add_item(ServerRef, Item) ->
	gen_server:cast(ServerRef, {add_item, Item}).

add_feed(ServerRef, RSS2Feed) ->
	Items = rss_parse:get_feed_items(RSS2Feed),
	lists:foreach(fun(Item) -> 
		add_item(ServerRef, Item)
	end, Items),
	ok.

- define (TIMEOUT, 1000).

get_all(ServerRef) ->
	gen_server:call(ServerRef, {get_all}, ?TIMEOUT).

subscribe(Queue1, Queue2) when is_pid(Queue1) ->
	gen_server:call(Queue2, {subscribe, Queue1}, ?TIMEOUT);

subscribe(Queue1, Queue2) ->
	Q1Pid = whereis(Queue1),
	case Q1Pid of
		undefined -> exit({noproc, {?MODULE, subscribe, [Queue1]}});
		_Else -> subscribe(Q1Pid, Queue2)
	end.

unsubscribe(Queue) ->
	gen_server:cast(Queue, {unsubscribe, self()}).

start(ServerName) ->
	gen_server:start({local, ServerName}, rss_queue_server, [], []).
start(ServerName, Url) ->
	gen_server:start({local, ServerName}, rss_queue_server, [Url], []).

preview_rss(Items) ->
	lists:foreach(fun(Item) ->
			Title = rss_parse:find_subelement(Item, title),
			io:format("~p~n", [Title])
		end, Items).

test() ->
	inets:start(),
	ssl:start(),
	{ok, _CnnPid} = start(cnn, "http://rss.cnn.com/rss/cnn_topstories.rss"),
	{ok, _BbcPid} = start(bbc, "http://newsrss.bbc.co.uk/rss/newsonline_world_edition/front_page/rss.xml"),
	{ok, _AlphaPid} = start(alpha, "https://seekingalpha.com/feed.xml"),
	{ok, _NewsPid} = start(news),
	{ok, _AllPid} = start(all),
	subscribe(news, cnn),
	subscribe(news, bbc),
	subscribe(all, alpha),
	subscribe(all, news),
	all.
