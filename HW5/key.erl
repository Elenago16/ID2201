-module(key).

-export([generate/0, between/3]).

generate() ->
	rand:uniform(1000000000).

between(Key, From, To) ->
	if 
		From < To ->
			if 
				(From < Key) and (Key =< To) -> true;
				true -> false
			end;
		From > To ->
			if 
				(From > Key) or (Key =< To) -> true;
				true -> false
			end;
		true ->
			true
	end.

