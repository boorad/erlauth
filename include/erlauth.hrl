-include_lib("webmachine/include/webmachine.hrl").

%% custom mime-type and accept header
%% http://barelyenough.org/blog/2008/05/versioning-rest-web-services/
-define(MIME_TYPE, "application/erlauth-v1+json").

-define(COOKIE, "AuthSession").

-define(t2b(T), term_to_binary(T)).
-define(b2t(B), binary_to_term(B)).
-define(l2b(L), list_to_binary(L)).
-define(b2l(B), binary_to_list(B)).
-define(l2a(L), list_to_atom(L)).
-define(a2l(A), atom_to_list(A)).
-define(l2i(L), list_to_integer(L)).
-define(i2l(I), integer_to_list(I)).

-record(user, {
          id,             %% usernum / userid
          user,           %% username
          hash,           %% password bcrypt hash
          cookie,         %% cookie hash
          profile,        %% ejson term, anything you want
          roles=[]        %% list of app roles (atoms)
         }).

-type body() :: list().
-type cookie() :: string().
-type authtype() :: creds_auth | cookie_auth.
-type user() :: #user{}.
