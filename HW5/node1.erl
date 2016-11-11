-module(node1).

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
    node(Id, Predecessor, Successor).

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

node(Id, Predecessor, Successor) ->
    receive
        {key, Qref, Peer} ->		% a peer needs to know our key
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        {notify, New} ->			% a new node informs us of its existence
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        {request, Peer} ->			% a predecessor needs to know our predecessor
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        {status, Pred} ->			% our successor informs us about its predecessor
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
		stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);
		probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor)
	end.

create_probe(Id, {_, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:monotonic_time()}.

forward_probe(Ref, T, Nodes, Id, {_Skey, Spid}) ->
    Spid ! {probe, Ref, [Id | Nodes], T}.

remove_probe(T, Nodes) ->
    Diff = erlang:monotonic_time()- T,
    io:format("Route: ~p~n", [lists:reverse(Nodes)]),
    io:format("Trip time: ~w micro~n", [Diff]).

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
		nil ->
			{Nkey, Npid};
        {Pkey,  _} ->
            case key:between(Nkey, Pkey, Id) of
				true -> {Nkey, Npid};
				false -> Predecessor
			end 
	end.

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