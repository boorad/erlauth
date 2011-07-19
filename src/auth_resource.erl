-module(auth_resource).

-export([init/1,
         allowed_methods/2,
         process_post/2
        ]).

-include("auth.hrl").

-define(COOKIE, "AuthSession").


%%
%% api
%%

init([]) -> {ok, undefined}.

allowed_methods(RD, Context) ->
  {['POST'], RD, Context}.

process_post(RD, Context) ->
  Body = mochiweb_util:parse_qs(wrq:req_body(RD)),
  Cookie = wrq:get_cookie_value(?COOKIE, RD),
  case auth:authenticate(Body, Cookie) of
    {ok, #user{user=User}} ->
      RD1 = set_cookie(User, RD),
      Resp = mochijson2:encode({struct,
                                [{user, list_to_binary(User)}]}),
      {true, wrq:append_to_response_body(Resp, RD1), Context};
    _Error ->
      Resp = mochijson2:encode({struct,
                                [{error, <<"forbidden">>},
                                 {reason, <<"Name or password is incorrect.">>}]}),
      {{halt, 403}, wrq:append_to_response_body(Resp, RD), Context}
  end.

%%
%% internal
%%

set_cookie(User, RD) ->
  Value0 = lists:append([User, ":", binary_to_list(crypto:sha(User))]),
  Value1 = strip_seps(base64:encode_to_string(Value0)),
  {K, V} = mochiweb_cookies:cookie(?COOKIE, Value1, [{path, "/"}]),
  wrq:set_resp_header(K, V, RD).

strip_seps(V) ->
  string:strip(V, right, $=).
