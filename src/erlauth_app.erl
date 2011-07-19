-module(erlauth_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback
start(_Type, _StartArgs) ->
  erlauth_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback
stop(_State) ->
  ok.
