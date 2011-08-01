# Erlauth

Authentication/Authorization Plugin for Webmachine

 * not completed yet, I wouldn't recommend using this.  If you must, take a look at its use in github.com/boorad/koth

released under Apache License, Version 2.0

# scratch

## REST API

### Endpoints

(relative paths)

**Endpoint** | **Method** | **Accept header / Content-Type**
------------- | ------------- | -------------
register | POST | application/erlauth-v1+json
login | POST | application/erlauth-v1+json
user/{user_id} | GET, PUT | application/erlauth-v1+json


### Dispatch
The paths above are relative i.e. you could do http://example.com/api/user/1 and erlauth resources are dispatched following /api

 * blurb on Joe's webmachine patch and movement of dispatch.conf



## Erlang API

Within Erlang, in your application's Webmachine resources the `is_authorized/2` functions are the ones that usually contain native Erlang API calls to Erlauth.  Simply provide the authz structure for the resource, Webmachine's RequestData (RD) variable, and the Context (Ctx).

An authz structure is a #erlauth_authz{} record, with its fields corresponding to http methods.

``` erlang
is_authorized(RD, Ctx) ->
  AuthZ = #erlauth_authz{get="everyone", put="this_user_and_admin"},
  erlauth:authz(AuthZ, RD, Ctx).
```

## Erlauth Data Stores

Data stores for use with Erlauth are swappable, and controlled in Erlauth's configuration.

 * file-based - for small sites, uses Erlang terms written to local disk
 * postgresql - rdbms option
 * memcachedb - in-memory store that may be persisted to disk

Schema creation...