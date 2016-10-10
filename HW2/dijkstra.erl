-module(dijkstra).

-export([table/2, route/2]).

entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
	{Node, Length, _} ->
	    Length;
	false ->
	    0
    end.

replace(Node, N , Gateway, Sorted) ->
	Unsorted = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway}),
  	lists:keysort(2, Unsorted).

update(Node, N, Gateway, Sorted) ->
   M = entry(Node, Sorted),
  if
    N < M -> replace(Node, N, Gateway, Sorted);
    true -> Sorted
  end.

%Map = [{berlin,[london,paris]}]
%List [{berlin, 2, paris}] = berlin could be reached in 2 hops using paris

iterate([], _, Table) ->   
    Table;
iterate([{_, inf, _}|_], _, Table) ->
    Table;
iterate([{Node, N, Gateway}|Sorted], Map, Table) ->
    case map:reachable(Node, Map) of
	[] ->
	    iterate(Sorted, Map, [{Node, Gateway}|Table]);
	Nodes ->
	    Updated = lists:foldl(fun(X, L) ->
					    update(X, N + 1, Gateway, L)
				    end, Sorted, Nodes),
	    iterate(Updated, Map, [{Node, Gateway}|Table])
    end.

table(Gateways, Map) ->
    Nodes = lists:usort(map:all_nodes(Map) ++ Gateways),
    Update = fun(Node) ->
					 {Node, inf, unknown}
             end,
    Initial = lists:map(Update, Nodes),
    UpdateGtw = fun(Gateway, Sorted) ->
                    update(Gateway, 1, Gateway, Sorted)
                end,
    Sorted = lists:foldl(UpdateGtw, Initial, Gateways),
    iterate(Sorted, Map, []).

route(Node, Table) ->
	case lists:keyfind(Node, 1, Table) of
    	false -> 
			notfound;
    	{_, Gateway} -> 
			{ok, Gateway}
  	end.
