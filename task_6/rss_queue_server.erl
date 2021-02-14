
- module (rss_queue_server).

% The server implements the gen_server behavior.
- behaviour (gen_server).
- export [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3].

% Additional helper functions exported by the callback module.
- export [start/1].
- include ("logging.hrl").

% TODO:  Implement module helper functions.

start(Name) -> gen_server:start({local, Name}, ?MODULE, [], []).


% TODO:  Behavior callback functions.  Implement the appropriate callback
%        functions as necessary for your server.

init([]) -> 
	{ok, {[], sets:new()}};
init([Url]) -> 
	RssPid = rss_reader:start(Url, self()),
	link(RssPid),
	{ok, {[], sets:new()}}.

handle_call({subscribe, QPid}, _From, State = {Queue, Subscribers}) -> 
	case sets:is_element(QPid, Subscribers) of
		true -> {reply, {error, already_subscribed}, State}; % ignore
		false -> 
			?INFO("Subscribe request: ~p~n", [QPid]),
			NewSubscribers = sets:add_element(QPid, Subscribers),
			erlang:monitor(process, QPid),
			lists:foreach(fun(Item) -> rss_queue:add_item(QPid, Item) end, Queue),
			{reply, {ok}, {Queue, NewSubscribers}}
	end;
handle_call({get_all}, _From, State = {Queue, _Subscribers}) ->
	{reply, Queue, State}.

handle_cast({add_item, RSSItem}, {Queue, Subscribers}) ->
	{Result, NewQueue} = rss_queue:try_add_item(RSSItem, Queue),
	case Result of
		updated ->
			lists:foreach(fun(QPid) -> rss_queue:add_item(QPid, RSSItem) end, 
				sets:to_list(Subscribers));
		same -> ok % skip
	end,
	{noreply, {NewQueue, Subscribers}};
handle_cast({unsubscribe, QPid}, State = {Queue, Subscribers}) ->
	case sets:is_element(QPid, Subscribers) of
		true -> 
			NewSubscribers = sets:del_element(QPid, Subscribers),
			% skip demonitoring
			{noreply, {Queue, NewSubscribers}};
		false -> {noreply, State} % ignore
	end.

handle_info({'DOWN', Ref, process, QPid, _}, State = {Queue, Subscribers}) ->
	case sets:is_element(QPid, Subscribers) of
		true -> 
			?INFO("Subscriber died: ~p~n", [QPid]),
			NewSubscribers = sets:del_element(QPid, Subscribers),
			erlang:demonitor(Ref),
			{noreply, {Queue, NewSubscribers}};
		false -> {noreply, State} % ignore
	end.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


