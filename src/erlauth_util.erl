-module(erlauth_util).

-export([get_value/2, get_value/3, get_config/1]).
-export([ensure_started/1, set_cookie/3]).

%% @doc get_value/2 and /3 replaces proplist:get_value/2 and /3
%%      this list function is a bit more efficient
%% @end
get_value(Key, List) ->
  get_value(Key, List, undefined).

get_value(Key, List, Default) ->
  case lists:keysearch(Key, 1, List) of
    {value, {Key,Value}} ->
      Value;
    false ->
      Default
  end.

get_config(Key) ->
  ConfigFile = code:priv_dir(koth) ++ "/auth.config",
  {auth_conf, Configs} = file:consult(ConfigFile),
  util:get_value(Key, Configs).

ensure_started(App) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end.

set_cookie(Cookie, User, RD) ->
  Value0 = lists:append([User, ":", binary_to_list(crypto:sha(User))]),
  Value1 = strip_seps(base64:encode_to_string(Value0)),
  {K, V} = mochiweb_cookies:cookie(Cookie, Value1, [{path, "/"}]),
  wrq:set_resp_header(K, V, RD).

%%
%% internal
%%

strip_seps(V) ->
  string:strip(V, right, $=).
