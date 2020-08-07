# Login with Okta Example

This example shows you how to use Haskell to login to your application with an Okta Hosted Login page.  The login is achieved through the [authorization code flow](https://developer.okta.com/authentication-guide/implementing-authentication/auth-code), where the user is redirected to the Okta-Hosted login page.  After the user authenticates they are redirected back to the application with an access code that is then exchanged for an access token.

This example shows you how to use Haskell to login to your application with a Custom Login page.  The login is achieved with the [Okta Sign In Widget][], which gives you more control to customize the login experience within your app.  After the user authenticates they are redirected back to the application with an authorization code that is then exchanged for an access token.

# Run

run this command to show help message regarding how to run the application

- `login-with-okta-exe --help`

# Build

- TBD

# Notes

## why not/cannot verify access Token

1. For Org AS

- Org AS's AccessToken aud is default to issuer
- `/keys` endpoint didn't publish key for verify AccessToken yet
- The whole point is Okta is the ONLY valid audience for AccessToken and it is Okta's job to verify AccessToken but not any other clients.

2. Custom AS's AccessToken `aud` is what admin customized, default to `api:default` which can be customized at Admin UI (Security -> API).
   AcessToken is intend to be used by your resource server hence shall be able to verify the AccessToken

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

## What if it is app used by many people?
