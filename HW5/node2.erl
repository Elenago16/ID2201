-module(node2).

-export([start/1, start/2]).

-define(Stabilize, 100).
-define(Timeout, 300).

start(Id) ->
    start(Id, nil).
start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(_, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
               {ok, {Skey, Peer}}
    after ?Timeout ->
            io:format("Time out: no response~n",[])
	end.

node(Id, Predecessor, Successor, Store) ->
    receive
        {key, Qref, Peer} ->		% a peer needs to know our key
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
        {notify, New} ->			% a new node informs us of its existence
            {Pred, Keep} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Keep);
        {request, Peer} ->			% a predecessor needs to know our predecessor
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        {status, Pred} ->			% our successor informs us about its predecessor
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);
		stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store);
		probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store);
		{add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);
		{lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);
		{handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged)

	end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			Added = storage:add(Key, Value, Store),
			Client ! {Qref, ok},
			Added;
		false ->
			Spid ! {add, Key, Value, Qref, Client},
			Store
	end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
	case key:between(Key,Pkey,Id) of
		true ->
			Result = storage:lookup(Key, Store),
			Client ! {Qref, Result};
		false ->
			{_, Spid} = Successor,
			Spid ! {lookup, Key, Qref, Client}
	end.

create_probe(Id, {_, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:monotonic_time()}.

forward_probe(Ref, T, Nodes, Id, {_Skey, Spid}) ->
    Spid ! {probe, Ref, [Id | Nodes], T}.

remove_probe(T, Nodes) ->
    Diff = erlang:monotonic_time()- T,
    io:format("Route: ~p~n", [lists:reverse(Nodes)]),
    io:format("Trip time: ~w micro~n", [Diff]).

notify({Nkey, Npid}, Id, Predecessor,Store) ->
	case Predecessor of
		nil -> 
			Keep = handover(Id, Store, Nkey, Npid),
			{{Nkey, Npid}, Keep};
		{Pkey, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					Keep = handover(Id, Store, Nkey, Npid),
					{{Nkey, Npid}, Keep};
				false ->
					{Predecessor,Store}
			end
	end.

handover(Id, Store, Nkey, Npid) ->
	{Keep, Rest} = storage:split(Nkey, Id, Store),
	Npid ! {handover, Rest},
	Keep.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
end.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.
stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
		nil -> 
			Spid ! {notify, {Id, self()}},
	    	Successor;
		{Id, _} -> Successor;
		{Skey, _} ->
			Spid ! {notify, {Id, self()}},
	    	Successor;
		{Xkey, Xpid} ->
    		case key:between(Xkey, Id, Skey) of
				true ->
					Xpid ! {request, self()},
		    		Pred;
				false ->
					Spid ! {notify, {Id, self()}},
		    		Successor
			end 
	end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).