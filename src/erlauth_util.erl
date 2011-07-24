-module(erlauth_util).

-export([get_value/2, get_value/3, get_config/1]).
-export([ensure_started/1, user_resp/1, set_cookie/3, get_cookie_hash/0]).
-export([to_hex/1]).

-include("erlauth.hrl").

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
  ConfigFile = code:priv_dir(erlauth) ++ "/auth.conf",
  {ok, Terms} = read_config_file(ConfigFile),
  [{auth_config, Configs}|_] = Terms,
  get_value(Key, Configs).

ensure_started(App) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end.

user_resp(#user{id=Id, user=Username, profile=Profile}) ->
  mochijson2:encode({struct,
                     [{id, Id},
                      {user, list_to_binary(Username)},
                      {profile, Profile}]}).

set_cookie(CookieName, #user{id=Id, cookie=CookieHash}, RD) ->
  Value0 = lists:append([?i2l(Id), ":", CookieHash]),
  Value1 = strip_seps(base64:encode_to_string(Value0)),
  {K, V} = mochiweb_cookies:cookie(CookieName, Value1, [{path, "/"}]),
  wrq:set_resp_header(K, V, RD).

%% get random cookie hash value
get_cookie_hash() ->
  BinRandom = crypto:rand_bytes(20),
  to_hex(BinRandom).

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

%%
%% internal
%%

read_config_file(ConfigFile) ->
  case file:consult(ConfigFile) of
    {ok, Terms} ->
      {ok, Terms};
    {error, enoent} ->
      throw({error, {config_file_not_found, ConfigFile}});
    Error ->
      throw({error, {unknown_config_error, Error}})
  end.

%% strip off separators from encoded cookie value
strip_seps(V) ->
  string:strip(V, right, $=).

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.
