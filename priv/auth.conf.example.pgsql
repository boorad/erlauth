%%-*- mode: erlang -*-
{auth_config, [
  {auth_store_module, erlauth_store_pgsql},
  {user_table_fields, [
    {user_id, "user_id"},
    {username, "username"},
    {password_hash, "password_hash"},
    {cookie_hash, "cookie_hash"},
    {profile, "profile"},
    {admin, "admin"}
  ]},
  {user_table, "users2"},
  {database, [
    {name, "koth"},
    {host, "localhost"},
    {port, 5432},
    {user, "koth"},
    {pass, "koth"}
  ]}
 ]}.
