-module(erlauth_user).

-export([get_user/2, set_user/2,
         set_cookie_hash/2]).

%%
%% api
%%

get_user(Field, User) ->
  execute(get_user, [Field, User]).

set_user(User, Hash) ->
  execute(set_user, [User, Hash]).

set_cookie_hash(UserId, CookieHash) ->
  execute(set_cookie_hash, [UserId, CookieHash]).

%%
%% internal
%%

execute(F,A) ->
  M = erlauth_util:get_config(auth_store_module),
  erlang:apply(M,F,A).
