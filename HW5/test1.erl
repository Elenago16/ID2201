-module(test1).

-export([start1/1, start2/1, start/0]).

start1(N) ->
	Pid = node1:start(1),
	lists:foreach(fun(M) -> node1:start(M, Pid)end, lists:seq(1, N)),
	timer:sleep(500),
  	Pid ! probe.

start2(N) ->
	Pid = node2:start(1),
	Name = 'node1',
    register(Name, Pid),
	lists:foreach(fun(M) -> node1:start(M, Pid)end, lists:seq(1, N)),
	Pid ! probe,
	timer:sleep(500),
  	Keys = test:keys(100),
	test:add(Keys, Pid),
	test:check(Keys, Pid).

start() ->
	Pid = node2:start(1),
	test:start(node2, 5, Pid),
	timer:sleep(500),
	Keys = test:keys(10),
	test:add(Keys, Pid),
	test:check(Keys, Pid).