-module(map).

-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    [].

update(Node, Links, Map) ->								
    lists:keystore(Node, 1, Map, {Node, Links}).


reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
	{_Name, Links} ->
	    Links;
	false ->
	    []
    end.

all_nodes(Map) ->
	lists:foldl(fun(N, L) -> add_node(N, L) end, [], Map).


add_node({Node, Links}, List) ->
	lists:foldl(fun(N, L) -> process_links(N, L) end, List, [Node|Links]).

process_links(Node, List) ->
	case lists:member(Node, List) of
		true ->
			List;
		false ->
			[Node|List]
	end.