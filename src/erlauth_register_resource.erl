-module(erlauth_register_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         malformed_request/2,
%         post_is_create/2,
         process_post/2
        ]).

-include("erlauth.hrl").

-define(REGISTER_FIELDS, ["register_username", "register_password1",
                          "register_password2", "register_profile"]).

%%
%% api
%%

init([]) -> {ok, undefined}.

allowed_methods(RD, Context) ->
  {['POST'], RD, Context}.

content_types_provided(RD, Ctx) ->
  {[{?MIME_TYPE, process_post}], RD, Ctx}.

%% so this seems like a shitshow :|
malformed_request(RD, Ctx) ->
  Body = mochiweb_util:parse_qs(wrq:req_body(RD)),
  A = lists:map(fun(F) ->
    erlauth_util:get_value(F, Body)
  end, ?REGISTER_FIELDS),
  Tests = [fun password_match/4, fun blank_fields/4],
  %% run validation tests on post body field values
  Results = lists:map(fun(F) -> erlang:apply(F, A) end, Tests),
  case strip_oks(Results) of
    []    -> {false, RD, Ctx};
    Fails -> {true, RD, [{validation_errors, Fails}|Ctx]}
  end.

process_post(_RD, _Ctx) ->
  ok.

%%
%% internal
%%

strip_oks(Results) ->
  io:format("results: ~p~n", [Results]),
  lists:filter(fun(R) -> (R =/= ok) end, Results).

password_match(_, _Pass1, _Pass1, _) ->
  ok;
password_match(_,_,_,_) ->
  "Password match failed".

blank_fields(undefined, _, _, _) ->
  "Blank username";
blank_fields(_, undefined, _, _) ->
  "Blank password";
blank_fields(_, _, undefined, _) ->
  "Blank password (again)";
blank_fields(_, _, _, undefined) ->
  "Blank profile";
blank_fields(_, _, _, _) ->
  ok.


%%
%% tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

password_match_test() ->
  ?assertEqual(ok, password_match(nil, "hey", "hey", nil)),
  ?assertEqual("Password match failed", password_match(nil, "hey", "guys", nil)),
  ok.

strip_oks_test() ->
  ?assertEqual([], strip_oks([ok, ok])).

-endif.
