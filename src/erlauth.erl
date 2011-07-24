-module(erlauth).

-export([start/0, stop/0, restart/0]).

-define(DEPS, [crypto, bcrypt]).

start() ->
  start_deps(),
  application:start(erlauth).

stop() ->
  application:stop(erlauth),
  stop_deps().

restart() ->
  stop(),
  start().

start_deps() ->
  lists:foreach(fun(Dep) -> util:ensure_started(Dep) end, ?DEPS).

stop_deps() ->
  lists:foreach(fun(Dep) -> util:ensure_started(Dep) end, lists:reverse(?DEPS)).
