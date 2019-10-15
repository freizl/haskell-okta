# Haskell + Login with Okta Example

This example shows you how to use Haskell to login to your application with an Okta Hosted Login page.  The login is achieved through the [authorization code flow](https://developer.okta.com/authentication-guide/implementing-authentication/auth-code), where the user is redirected to the Okta-Hosted login page.  After the user authenticates they are redirected back to the application with an access code that is then exchanged for an access token.

This example shows you how to use Haskell to login to your application with a Custom Login page.  The login is achieved with the [Okta Sign In Widget][], which gives you more control to customize the login experience within your app.  After the user authenticates they are redirected back to the application with an authorization code that is then exchanged for an access token.

# Notes

## why not/cannot verify access Token

- Custom AS's AccessToken aud is what admin customized, default to `api:default`
- Org AS's AccessToken aud is default to issuer
- `/keys` endpoint didn't publish key for verify AccessToken yet
- The whole point is Okta is the ONLY valid audience for AccessToken and it is Okta's job to verify AccessToken but not any other clients.
