-module(auth_resource).

-export([init/1,
         allowed_methods/2,
         process_post/2
        ]).

-include("erlauth.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%
%% api
%%

init([]) -> {ok, undefined}.

allowed_methods(RD, Context) ->
  {['POST'], RD, Context}.

process_post(RD, Context) ->
  Body = mochiweb_util:parse_qs(wrq:req_body(RD)),
  Cookie = wrq:get_cookie_value(?COOKIE, RD),
  case authn:authenticate(Body, Cookie) of
    {ok, creds_auth, User0=#user{id=UserId}} ->
      NewCookieHash = erlauth_util:get_cookie_hash(),
      User = User0#user{cookie=NewCookieHash},
      RD1 = erlauth_util:set_cookie(?COOKIE, User, RD),
      ok = erlauth_user:set_cookie_hash(UserId, NewCookieHash),
      Resp = erlauth_util:user_resp(User),
      {true, wrq:append_to_response_body(Resp, RD1), Context};
    {ok, cookie_auth, User=#user{}} ->
      Resp = erlauth_util:user_resp(User),
      {true, wrq:append_to_response_body(Resp, RD), Context};
    Error ->
      io:format("process_post error: ~p~n", [Error]),
      Resp = mochijson2:encode(
               {struct,
                [{error, <<"forbidden">>},
                 {reason, <<"Name or password is incorrect.">>}]}),
      {{halt, 403}, wrq:append_to_response_body(Resp, RD), Context}
  end.
