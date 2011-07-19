-module(auth_store_file).

-export([get_user/1, set_user/2,
         get_profile/1, set_profile/2]).

%%
%% api
%%

get_user(User) ->
  File = get_store_file(),
  case file:is_file(File) of
    true ->
      lookup_user(File, User);
    false ->
      file:write_file(File, [])
  end.

set_user(_User, _Hash) ->
  ok.

get_profile(_User) ->
  ok.

set_profile(_User, _Profile) ->
  ok.

%%
%% internal
%%

get_store_file() ->
  code:priv_dir(erlauth) ++ "/" ++ util:get_config(auth_file).

lookup_user(File, User) ->
  {ok, Users} = file:consult(File),
  Hash = util:get_value(User, Users),
  {User, Hash}.
