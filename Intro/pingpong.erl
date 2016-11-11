-module(pingpong).

-export([start_ping/1, start_pong/0, ping/1, pong/0]).

ping(Pong_Node) ->
	{pong, Pong_Node} ! self(), % Passing the Pid, not registered name!

receive
      "hello there" -> 
		  io:format("I got the message!~n")
end.

pong() ->
    receive
      X -> 
		  X ! "hello there"
end.

start_pong() ->
    register(pong, spawn(pingpong, pong, [])).

start_ping(Pong_Node) ->
    spawn(pingpong, ping, [Pong_Node]).
