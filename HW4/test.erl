-module(test).

-export([first/3, add/4, more/3, freeze/1, go/1, sleep/2, stop/1]).

% Used to create the first worker, try:
% W1 = test:first(1, gms1, 1000)

first(N, Module, Sleep) ->
   worker:start(N, Module, rand:uniform(256), Sleep).

% Used to create additional workers, try:
%  W2 = test:add(2, gms1, W1, 1000) and 
%  W3 = test:add(3, gms1, W1, 1000) and ...

add(N, Module, Wrk, Sleep) ->
   worker:start(N, Module, rand:uniform(256), Wrk, Sleep).

% To create a number of workers in one go,
% test:more(5, gms1, 1000).

more(N, Module, Sleep) when N > 1 ->
    Wrk = first(1, Module, Sleep),
    Ns = lists:seq(2,N),
    lists:map(fun(Id) -> add(Id, Module, Wrk, Sleep) end, Ns),
    Wrk.
		      

% These are messages that we can send to one of the workers. It will
% multicast it to all workers. They should (if everything works)
% receive the message at the same (logical) time.

% test:freeze(W1).
freeze(Wrk) ->            
    Wrk ! {send, freeze}.

% test:go(W1).
go(Wrk) ->
    Wrk ! {send, go}.	 

% test:sleep(W1,1000).
sleep(Wrk, Sleep) ->      
    Wrk ! {send, {sleep, Sleep}}.

% test:stop(W1).
stop(Wrk) ->
    Wrk ! {send, stop}.