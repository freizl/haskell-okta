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
  - current sharing via session cookie, which is hack and cumbersome

- start with only allow single config profile. every time user want different combination would reset.
  - still be able to reuse config type (OktaSampleState, etc)
  - need a form submit to change the state

- [X] then figure out a store solution (in memory or yaml or dhall?) in order to support multiple config profile.
  - but how to determine which one to use (active) config profile?

- Rich server and thin client
  - allows creates multiple profiles for single user
  - set particular one as active profile
  - render profile in left pane and user info or siw in right pane
  - run code flow which finish in server side, save tokens, get user info (same as sample today)
    - log the entire transaction from server side and show in client after.
    - same decent effort here.
  - run implicit flow from client (same as today)
    - send tokens/user info to server via API (instead of sharing via cookie as today)
    - maybe just send token, so server can do validation same as code flow
  - implement UI authjs methods like renew, getUserInfo etc (buttons?)
    - the good part is requests are visual in browser network pane

- OR make rich client and stateless server
  - left pane to create config OIDC profiles
  - right pane to show widget or profile after login
  - ask user to save which saves to localStorage
  - or download config profiles and upload profiles (JSON, yaml)
  - server implements code flow
  - server takes code/token and a profile
  - with this, there is no problem to support multiple profiles, multiple users.

## What if it is app used by many people?

- need primary key for Who is using of which config profile
