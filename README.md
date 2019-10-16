[![Build Status](https://secure.travis-ci.org/freizl/haskell-okta.svg?branch=master)](https://travis-ci.com/freizl/haskell-okta)

# Sample config

TODO: yaml or dhall; passed as parameter

```
{
  "webServer": {
    "oidc": {
      "issuer": "https://{domain}/oauth2/default",
      "scope": "openid profile email",
      "client_id": "xx",
      "client_secret": "yy",
      "redirect_uri": "http://localhost:8080/authorization-code/callback"
    },
    "port": 8080,
    "resourceServer": {
      "messagesUrl": "http://localhost:8000/api/messages"
    }
  }
}
```

# TODO

- [X] unit test is broken
- [X] use `.well-known` endpoint to fetch authenticate url and issuer.
- [x] merge `custom-login` and `okta-hosted-login`
- [ ] fix unit test
- [ ] clear cookie that set by SIW when start up
- [ ] add stylish-haskell and hlint as dependencies
- [ ] sample app shall be able to paramaters for run against different issuer
- [ ] show ID token in web page
