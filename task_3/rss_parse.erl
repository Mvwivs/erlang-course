
- module rss_parse.
- export [is_rss2_feed/1, get_feed_items/1, get_item_time/1, compare_feed_items/2, find_subelement/2].
- export [test/0].
- include_lib("xmerl/include/xmerl.hrl").

is_rss2_feed(RssFeed) ->
	{Root, _} = RssFeed,
	Attributes = Root#xmlElement.attributes,
	lists:any(fun(Attr) ->
			(Attr#xmlAttribute.name =:= version) and (Attr#xmlAttribute.value =:= "2.0")
		end, Attributes).

get_feed_items(RSS2Feed) ->
	get_items(RSS2Feed).

get_items(#xmlElement{content = Content}) ->
	case length(Content) of
		0 -> [];
		_Else ->
			lists:append(
				lists:map(fun get_item/1, Content))
	end;
get_items(_Node) -> [].

get_item(Element) ->
	case Element of
		#xmlElement{name = Name} when Name =:= item ->
			case get_item_time(Element) of
				bad_date -> []; % skip items without date
				_Else -> [Element]
			end;
		_Else -> get_items(Element)
	end.

get_item_time(Item) ->
	Date = find_subelement(Item, pubDate),
	case Date of 
		false -> bad_date;
		_Else ->
			ErlangDate = httpd_util:convert_request_date(Date),
			if
				ErlangDate =:= bad_date -> bad_date;
				true -> calendar:datetime_to_gregorian_seconds(ErlangDate)
			end
	end.

% @private
% @doc Given an XML element of some kind, this helper function will go through
%      and remove any details about other XML elements, for example in the
%      "parents" or "pos" fields.
%
% @spec extract_xml(Node::xmlAny()) -> xmlAny()
%
extract_xml(Elem = #xmlElement{}) ->
    Elem#xmlElement{parents=[], pos=0, xmlbase=undeclared,
        content=lists:map(fun extract_xml/1, Elem#xmlElement.content),
        attributes=lists:map(fun extract_xml/1, Elem#xmlElement.attributes)};
extract_xml(Attr = #xmlAttribute{}) ->
    Attr#xmlAttribute{parents=[], pos=0};
extract_xml(Text = #xmlText{}) ->
    Text#xmlText{parents=[], pos=0};
extract_xml(Comment = #xmlComment{}) ->
    Comment#xmlComment{parents=[], pos=0};
extract_xml(Other) ->
    Other.

find_subelement(Elem, Subelement) ->
	Sub = lists:keyfind(Subelement, #xmlElement.name, Elem#xmlElement.content),
	case Sub of
		false -> false;
		_Else -> 
			SubContent = hd(Sub#xmlElement.content),
			SubContent#xmlText.value
	end.

compare_feed_items(OldItem, NewItem) ->
	OldItemStripped = extract_xml(OldItem),
	NewItemStripped = extract_xml(NewItem),
	case OldItemStripped =:= NewItemStripped of
		true -> same;
		_Else ->
			GuidRes = compare_subelement(OldItemStripped, NewItemStripped, guid),
			case GuidRes of
				updated -> updated_guid;
				_Else ->
					TitleRes = compare_subelement(OldItemStripped, NewItemStripped, title),
					case TitleRes of
						updated -> updated_title;
						_Else ->
							LinkRes = compare_subelement(OldItemStripped, NewItemStripped, link),
							case LinkRes of
								updated -> [updated_link, LinkRes];
								_Else -> different
							end
					end
			end
	end.

compare_subelement(OldItem, NewItem, Subelement) ->
	OldSubelement = find_subelement(OldItem, Subelement),
	NewSubelement = find_subelement(NewItem, Subelement),
	case (OldSubelement =:= false) or (NewSubelement =:= false) of
		true -> false;
		_Else -> 
			case OldSubelement =:= NewSubelement of
				true -> updated;
				_Else -> false
			end
	end.

test() ->
	{Rss, _} = xmerl_scan:file("test.xml"),
	Items = rss_parse:get_feed_items(Rss),
	Old = extract_xml(lists:nth(1, Items)),
	New = extract_xml(lists:nth(2, Items)),
	io:format("~p~n~n~n~p~n", [Old, New]),
	Old =:= New.
