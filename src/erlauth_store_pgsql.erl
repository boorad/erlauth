-module(erlauth_store_pgsql).

-export([get_user/2, update_user/3, add_user/1]).

-include("erlauth.hrl").

%%
%% api
%%
-spec get_user(atom(), string()) -> {ok, #user{}} | nil.
get_user(id, Id) ->
  Query = "SELECT " ++ user_table_fields() ++ " "
    "FROM " ++ user_table() ++ " "
    "WHERE " ++ user_table_field(user_id) ++ "=$1",
  {ok, _Cols, Rows} = equery(Query, [Id]),
  case Rows of
    [Row|_] -> make_user(Row);
    _ -> nil
  end;
get_user(name, Name) ->
  Query = "SELECT " ++ user_table_fields() ++ " "
    "FROM " ++ user_table() ++ " "
    "WHERE " ++ user_table_field(username) ++ "=$1",
  {ok, _Cols, Rows} = equery(Query, [Name]),
  case Rows of
    [Row|_] -> make_user(Row);
    _ -> nil
  end.

update_user(Field, UserId, Value) ->
  Query = "UPDATE " ++ user_table() ++ " "
    "SET " ++ user_table_field(Field) ++ "=$1 "
    "WHERE " ++ user_table_field(user_id) ++ "=$2;",
  {ok, 1} = equery(Query, [Value, UserId]),
  ok.

add_user(#user{user=Username, hash=PassHash, cookie=CookieHash, profile=Profile,
               admin=Admin}) ->
  Fields0 = user_table_fields(),
  IdField = user_table_field(user_id),
  Fields1 = case string:str(Fields0, IdField) of
    0 -> Fields0;
    1 -> string:sub_string(Fields0, length(IdField))
  end,
  Query = "INSERT INTO " ++ user_table() ++ " (" ++ Fields1 ++ ") values "
    "($1, $2, $3, $4, $5)",
  io:format("Query: ~p~n", [Query]),
  {ok, 1} = equery(Query, [Username, PassHash, CookieHash, Profile, Admin]),
  ok.

%%
%% internal
%%

make_user({Id, Username, PassHash, CookieHash, Profile, Admin}) ->
  {ok, #user{id=Id, user=?b2l(Username), hash=?b2l(PassHash),
             cookie=?b2l(CookieHash), profile=mochijson2:decode(Profile),
             admin=Admin}}.

db_conn() ->
  DbConfig = erlauth_util:get_config(database),
  Host = erlauth_util:get_value(host, DbConfig),
  User = erlauth_util:get_value(user, DbConfig),
  Name = erlauth_util:get_value(name, DbConfig),
  Port = erlauth_util:get_value(port, DbConfig),
  Opts = [{database, Name}, {port, Port}],
  pgsql:connect(Host, User, Opts).

%% squery(Query) ->
%%   {ok, C} = db_conn(),
%%   Result = pgsql:squery(C, Query),
%%   ok = pgsql:close(C),
%%   Result.

equery(Query, Args) ->
  {ok, C} = db_conn(),
  Result = pgsql:equery(C, Query, Args),
  ok = pgsql:close(C),
  Result.

%% get_fields(Cols) ->
%%   lists:map(fun({column, Field, _, _, _, _}) -> Field end, Cols).

user_table_fields() ->
  Fields = erlauth_util:get_config(user_table_fields),
  {_Keys, Fieldnames} = lists:unzip(Fields),
  string:join(Fieldnames, ", ").

user_table_field(Key) ->
  Fields = erlauth_util:get_config(user_table_fields),
  {Key, Value} = lists:keyfind(Key, 1, Fields),
  Value.

user_table() ->
  erlauth_util:get_config(user_table).
