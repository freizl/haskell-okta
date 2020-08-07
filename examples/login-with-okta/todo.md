# Document Title
# TODO

## Refactoring (change to as playground)

1. specify parameters from UI (redirect URI, scopes, response type etc)
  - need to 'remember' the parameters for `/callback` endpoint,
    which could have either `code` or `id_token`, `access_token`
2. be able to generate `/authorize` url and redirect to okta for login
3. redirect to customized siw page
  - use saved parameters at step 1 to render widget
  - on success, widget may redirect to `/callback` endpoint
  - or call widget's `successHandler`
4. callback handlers for 
  - `web code` flow (exchange code for tokens in server side)
  - implicit/pkce flow (exchange code in client side or just obtain tokens from uri)

### Implementation

- shall change to API driven project first?
- how to share token after user login?
  - token can obtain from server during web code flow
  - also from client during implicit flow
  - current sharing via sesison cookie, which is hack and cubersome

- start with only allow single config profile. every time user want different combination would reset.
  - still be able to reuse config type (OktaSampleState, etc)
  - need a form submit to change the state
  
- then figure out a store solution (in memory or yaml or dhall?) in order to support multiple config profile.
  - but how to determine which one to use (active) config profile?
 

## What if it is app used by many people?

- need primary key for Who is using of which config profile

