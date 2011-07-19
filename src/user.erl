-module(user).

-export([get_user/1, set_user/2,
         get_profile/1, set_profile/2]).

%%
%% api
%%

get_user(User) ->
  execute(get_user, [User]).

set_user(User, Hash) ->
  execute(set_user, [User, Hash]).

get_profile(User) ->
  execute(get_profile, [User]).

set_profile(User, Profile) ->
  execute(set_profile, [User, Profile]).

%%
%% internal
%%

get_config(Key) ->
  ConfigFile = code:priv_dir(koth) ++ "/auth.config",
  {ok, Terms} = file:consult(ConfigFile),
  [{auth_conf, Configs}|_] = Terms,
  util:get_value(Key, Configs).

execute(F,A) ->
  M = get_config(auth_store_module),
  erlang:apply(M,F,A).
