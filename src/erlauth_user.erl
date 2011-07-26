-module(erlauth_user).

-export([get_user/2, update_user/3, add_user/1]).

%%
%% api
%%

get_user(Field, User) ->
  execute(get_user, [Field, User]).

update_user(Field, UserId, Value) ->
  execute(update_user, [Field, UserId, Value]).

add_user(User) ->
  execute(add_user, [User]).

%%
%% internal
%%

execute(F,A) ->
  M = erlauth_util:get_config(auth_store_module),
  erlang:apply(M,F,A).
