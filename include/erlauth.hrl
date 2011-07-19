
-record(user, {
          user,
          hash
         }).

-record(userprofile, {
          user,
          info
         }).

-type body() :: list().
-type cookie() :: string().
