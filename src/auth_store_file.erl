-module(auth_store_file).

-export([get_user/2, set_user/2,
         set_cookie_hash/2]).

-include("erlauth.hrl").

%%
%% api
%%

get_user(id, Id) ->
  file_op(fun lookup_user/3, [2, Id]);
get_user(name, Name) ->
  file_op(fun lookup_user/3, [3, Name]).

set_user(User, Hash) ->
  file_op(fun write_user/3, [User, Hash]).

set_cookie_hash(UserId, CookieHash) ->
  file_op(fun write_cookie_hash/3, [UserId, CookieHash]).

%%
%% internal
%%

get_store_file() ->
  code:priv_dir(erlauth) ++ "/" ++ erlauth_util:get_config(auth_file).

file_op(F,A) ->
  File = get_store_file(),
  case filelib:is_file(File) of
    true ->
      erlang:apply(F, [File | A]);
    false ->
      file:write_file(File, [])
  end.

lookup_user(File, Field, Value) ->
  {ok, Terms} = file:consult(File),
  [{users, Users}|_] = Terms,
  case lists:keyfind(Value, Field, Users) of
    {user, Id, Username, Hash, Cookie, Profile, Admin} ->
      {ok, #user{id=Id, user=Username, hash=Hash, cookie=Cookie,
                 profile=Profile, admin=Admin}};
    false ->
      {error, {user_not_found, Value}}
  end.

%% hash = bcrypt password hash
%% TODO: test me
write_user(File, User, Hash) ->
  {ok, Terms} = file:consult(File),
  [{users, Users}|_] = Terms,
  NewUsers = lists:keystore(User, 1, Users, {User, Hash}),
  write_users(File, NewUsers).

write_users(File, Users) ->
  Data = {users, Users},
  ok = file:write_file(File, io_lib:format("~p.~n", [Data])).

write_cookie_hash(File, UserId, CookieHash) ->
  io:format("~nFile: ~p~nUserId: ~p~nCookieHash: ~p~n~n", [File, UserId, CookieHash]),
  {ok, Terms} = file:consult(File),
  [{users, Users}|_] = Terms,
  {user, UserId, Name, Hash, _, Profile, Admin} = lists:keyfind(UserId,2,Users),
  NewUsers = lists:keystore(UserId, 2, Users,
      {user, UserId, Name, Hash, CookieHash, Profile, Admin}),
  write_users(File, NewUsers).
