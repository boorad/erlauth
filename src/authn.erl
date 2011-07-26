-module(authn).

-export([authenticate/2, add_user/2]).

-include("erlauth.hrl").
-include_lib("eunit/include/eunit.hrl").

%%
%% api
%%

-spec authenticate(body(), cookie()) -> {ok, authtype(), #user{}} | forbidden.
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
      [Id, ReqCookieHash] ->
        {ok,User=#user{cookie=CookieHash}} = erlauth_user:get_user(id,?l2i(Id)),
        case CookieHash of
          ReqCookieHash ->
            {ok, cookie_auth, User};
          _ ->
            io:format("cookie hash expired or invalid~nRequested: ~p~n"
                      "In auth store: ~p~n", [ReqCookieHash, CookieHash]),
            authenticate(Body, undefined)
        end;
      _ ->
        authenticate(Body, undefined)
    end
  catch
    _:Err ->
      io:format("cookie auth error: ~p~n~p~n", [Err, erlang:get_stacktrace()]),
      authenticate(Body, undefined)
  end.

add_user(User0=#user{}, Pass) ->
  {ok, Salt} = bcrypt:gen_salt(),
  {ok, Hash} = bcrypt:hashpw(Pass, Salt),
  User = User0#user{hash=Hash},
  erlauth_user:add_user(User).

%%
%% internal
%%

%% try password against pwd_hash in #user and yay/nay this authn request
int_authn(User, Pass) ->
  try
    {ok, Return=#user{hash=Hash}} = erlauth_user:get_user(name, User),
    case bcrypt:hashpw(Pass, Hash) of
      {ok, Hash} -> {ok, creds_auth, Return};
      _          -> forbidden
    end
  catch
    Cls:Err ->
      io:format("~nauthn error: ~p ~p~n~p~n",
                [Cls, Err, erlang:get_stacktrace()]),
      forbidden
  end.


get_creds(Body) ->
  try
    {ok,
     erlauth_util:get_value("username", Body),
     erlauth_util:get_value("password", Body)}
  catch
    _:Error ->
      {error, Error}
  end.
