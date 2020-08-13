Login with Okta Example

# Run

run this command to show help message regarding how to run the application

- `login-with-okta-exe --help`

# Build

- TBD

# Notes

## why not/cannot verify access Token

1. For Org AS

- Org AS's AccessToken aud is default to issuer
- `/keys` endpoint didn't publish key for verify AccessToken
- The whole point is that Okta is the ONLY valid audience for AccessToken
  and it is Okta's job (as resource server) to verify AccessToken.
- Hence AT shall be treated as opaque.

2. For Custom AS

- `aud` is default to `api:default` which can be customized at Okta Admin Console (Security -> API).
- AcessToken is intend to be used by your resource server hence your resource server shall be able to
  verify it. (check on `aud`)

3. See [this reference](https://developer.okta.com/docs/concepts/auth-servers/)
