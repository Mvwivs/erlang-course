
%% @doc RSS Queue module. Periodically retrieves items from RSS feed, allows feed aggregation.
%% @author Mazov Vladimir

- module (rss_queue).
- export [start/0, start/1, init/1].
- export [add_item/2, add_feed/2, get_all/1].
- export [test/0].
- include ("logging.hrl").

%% @doc Server loop. Receives messages from clients and sends replies.
server(State = {Queue, Subscribers}) ->
	receive 
		{add_item, RSSItem} -> 
			{Result, NewQueue} = try_add_item(RSSItem, Queue),
			case Result of
				updated ->
					lists:foreach(fun(QPid) -> add_item(QPid, RSSItem) end, 
						sets:to_list(Subscribers));
				same -> ok % skip
			end,
			server({NewQueue, Subscribers});
		{get_all, ReqPid} -> 
			ReqPid ! {get_all, Queue},
			server(State);
		{subscribe, QPid} ->
			case sets:is_element(QPid, Subscribers) of
				true -> server(State); % ignore
				false -> 
					?INFO("Subscribe request: ~p~n", [QPid]),
					NewSubscribers = sets:add_element(QPid, Subscribers),
					erlang:monitor(process, QPid),
					lists:foreach(fun(Item) -> add_item(QPid, Item) end, Queue),
					server({Queue, NewSubscribers})
			end;
		{unsubscribe, QPid} -> 
			case sets:is_element(QPid, Subscribers) of
				true -> 
					NewSubscribers = sets:del_element(QPid, Subscribers),
					% skip demonitoring
					server({Queue, NewSubscribers});
				false -> server(State) % ignore
			end;
		{'DOWN', Ref, process, QPid, _} ->
			case sets:is_element(QPid, Subscribers) of
				true -> 
					?INFO("Subscriber died: ~p~n", [QPid]),
					NewSubscribers = sets:del_element(QPid, Subscribers),
					erlang:demonitor(Ref),
					server({Queue, NewSubscribers});
				false -> server(State) % ignore
			end
	end.
%% @doc Starts default server.
server() ->
	server({[], sets:new()}).

%% @doc Tries to add rss item to queue if possible. Ignores if item already exists.
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

%% @doc Checks if rss item is present in queue.
check_items(_Item, [], _Pos) -> different;
check_items(Item, [Old | Rest], Pos) ->
	Res = rss_parse:compare_feed_items(Old, Item),
	case Res of
		updated -> {updated, Pos};
		same -> same;
		_Else -> check_items(Item, Rest, Pos + 1)
	end.

%% @doc Compares two rss items by their publish date.
pub_date_comp(Item1, Item2) ->
	Time1 = rss_parse:get_item_time(Item1),
	Time2 = rss_parse:get_item_time(Item2),
	Time1 > Time2.

%% @doc Removes item from list by index.
remove_from_list(Index, List) ->
	{Left, [_|Right]} = lists:split(Index - 1, List),
	Left ++ Right.


%% @doc Adds item to running rss_queue server
add_item(QPid, Item) when is_pid(QPid) ->
	QPid ! {add_item, Item}.

%% @doc Extracts all items from feed and adds them to running rss_queue server
add_feed(QPid, RSS2Feed) when is_pid(QPid) ->
	Items = rss_parse:get_feed_items(RSS2Feed),
	lists:foreach(fun(Item) -> 
		add_item(QPid, Item)
	end, Items),
	ok.

- define(TIMEOUT, 1000).

%% @doc Reads all items from queue.
get_all(QPid) when is_pid(QPid) ->
	QPid ! {get_all, self()},
	receive
		{get_all, List} -> {ok, List}
	after ?TIMEOUT -> {error, timeout}
	end.

%% @doc Spawns rss_queue server.
start() ->
	spawn(rss_queue, init, [[]]).

%% @doc Spawns rss_queue with specified URL.
start(Url) ->
	spawn(rss_queue, init, [[Url]]).

%% @doc Initialize rss_queue. If URL is provided, starts queue with rss_reader.  
init([]) ->
	server();
init([Url]) -> 
	RssPid = rss_reader:start(Url, self()),
	link(RssPid),
	server().

%% @doc Simple test case. Starts 3 rss_readers and 2 aggregating queues.
- spec test() -> pid().
test() ->
	inets:start(),
	ssl:start(),
	CnnPid = start("http://rss.cnn.com/rss/cnn_topstories.rss"),
	BbcPid = start("http://newsrss.bbc.co.uk/rss/newsonline_world_edition/front_page/rss.xml"),
	VedomostiPid = start("https://www.vedomosti.ru/rss/rubric/economics"),
	NewsPid = start(),
	AllPid = start(),
	CnnPid ! {subscribe, NewsPid},
	BbcPid ! {subscribe, NewsPid},
	VedomostiPid ! {subscribe, AllPid},
	NewsPid ! {subscribe, AllPid},
	AllPid.
