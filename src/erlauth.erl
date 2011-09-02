-module(erlauth).

%% api
-export([start/0, stop/0, restart/0]).
-export([authn/0, authz/2, get_user/2]).

-define(DEPS, [crypto, bcrypt]).

-include("erlauth.hrl").

%%
%% api
%%

start() ->
  start_deps(),
  application:start(erlauth).

stop() ->
  application:stop(erlauth),
  stop_deps().

restart() ->
  stop(),
  start().

%% TODO: move here, or make call to authn module
authn() ->
  ok.

%% check user's roles against required roles and return true if authorized
%% i.e. user's roles have some intersection with required roles
%% TODO: authz/3 with 3rd arg 'all' means you need all req'd roles to authz?
-spec authz(user() | undefined, list()) -> boolean().
authz(undefined, _) -> false;
authz(#user{roles=UserRoles}, RequiredRoles) when is_list(UserRoles) ->
  intersection(UserRoles, RequiredRoles) =/= [];
authz(_, _) -> false.

get_user(RD, Ctx) ->
  get_user(ctx, RD, Ctx).

%%
%% internal
%%

start_deps() ->
  lists:foreach(fun(Dep) -> erlauth_util:ensure_started(Dep) end, ?DEPS).

stop_deps() ->
  lists:foreach(fun(Dep) -> erlauth_util:ensure_started(Dep) end,
                lists:reverse(?DEPS)).

intersection(L1,L2) ->
  lists:filter(fun(X) -> lists:member(X,L1) end, L2).

%% get user from Context
get_user(ctx, RD, Ctx) ->
  case erlauth_util:get_value(user, Ctx) of
    undefined ->
      get_user(rd, RD, Ctx);
    User -> User
  end;
%% get user from RequestData
get_user(rd, RD, Ctx) ->
  Body = mochiweb_util:parse_qs(wrq:req_body(RD)),
  Cookie = wrq:get_cookie_value(?COOKIE, RD),
  case authn:authenticate(Body, Cookie) of
    {ok, _AuthType, User} ->
      erlauth_util:add_to_context({user, User}, Ctx),
      User;
    _ ->
      undefined
  end.

%%
%% tests
%%

-include_lib("eunit/include/eunit.hrl").

authz_test() ->
  ?assert(authz(#user{roles=[admin]}, [admin])),
  ?assertNot(authz(#user{}, [admin])),
  ?assertNot(authz(hey,guys)),
  ok.

intersection_test() ->
  ?assertEqual([admin], intersection([admin], [admin])),
  ?assertEqual([], intersection([], [something])),
  ok.
