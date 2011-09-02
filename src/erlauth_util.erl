-module(erlauth_util).

-export([get_value/2, get_value/3, get_config/1]).
-export([ensure_started/1, user_resp/1, set_cookie/3, get_cookie_hash/0]).
-export([add_to_context/2]).
-export([to_binary/1, to_int/1, to_list/1, to_float/1, to_atom/1, to_hex/1]).

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

add_to_context({Key, Value}, Ctx) when is_list(Ctx) ->
  lists:keystore(Key, 1, Ctx, {Key, Value});
add_to_context(Term, _Ctx) ->
  %% not sure what's in context here if it's not a list,
  %% so starting over with fresh one.
  [Term].


to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_binary(undefined)            -> undefined;
to_binary(V) when is_integer(V) -> to_binary(?i2l(V));
to_binary(V) when is_list(V)    -> to_binary(?l2b(V));
to_binary(V) when is_float(V)   -> to_binary(float_to_list(V));
to_binary(V) when is_binary(V)  -> V.

to_int(undefined)            -> undefined;
to_int(V) when is_float(V)   -> round(V);
to_int(V) when is_integer(V) -> V;
to_int(V) when is_list(V)    -> ?l2i(V);
to_int(V) when is_binary(V)  -> to_int(?b2l(V)).

to_list(undefined)            -> undefined;
to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(V) when is_list(V)    -> V;
to_list(V) when is_binary(V)  -> ?b2l(V);
to_list(V) when is_atom(V)    -> ?a2l(V).

to_float(undefined)            -> undefined;
to_float(V) when is_integer(V) -> V + 0.0;
to_float(V) when is_list(V)    -> list_to_float(V);
to_float(V) when is_binary(V)  -> to_float(?b2l(V)).

to_atom(undefined)         -> undefined;
to_atom(V) when is_atom(V) -> V;
to_atom(V) when is_list(V) -> list_to_atom(V);
to_atom(V)                 -> to_atom(to_list(V)).

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
