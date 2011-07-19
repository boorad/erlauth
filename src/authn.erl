-module(authn).

-export([authenticate/2, add_user/2]).

-include("auth.hrl").

%%
%% api
%%

-spec authenticate(body(), cookie()) -> {ok, #userprofile{}} | forbidden.
authenticate(no_body, undefined) ->
  forbidden;
authenticate(Body, undefined) ->
  case get_creds(Body) of
    {ok, User, Pass} ->
      int_authn(User, Pass);
    _ ->
      forbidden
  end;
authenticate(Body, Cookie) ->
  try
    StrCookie = base64:decode_to_string(Cookie),
    case string:tokens(StrCookie, ":") of
      [User|_] -> user:get_profile(User);
      _        -> authenticate(Body, undefined)
    end
  catch
    _:_ -> authenticate(Body, undefined)
  end.

add_user(User, Pass) ->
  {ok, Salt} = bcrypt:gen_salt(),
  {ok, Hash} = bcrypt:hashpw(Pass, Salt),
  user:set_user(auth, User, Hash).

%%
%% internal
%%

int_authn(User, Pass) ->
  try
    {ok, #user{hash=Hash}} = user:get_user(User),
    case bcrypt:hashpw(Pass, Hash) of
      {ok, Hash} -> user:get_profile(User);
      _          -> forbidden
    end
  catch
    Class:Err ->
      io:format("~nauthn error: ~p:~p~n", [Class, Err]),
      forbidden
  end.


get_creds(Body) ->
  try
    {ok, util:get_value("username", Body), util:get_value("password", Body)}
  catch
    _:Error ->
      {error, Error}
  end.
