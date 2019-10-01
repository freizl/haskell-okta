[![Build Status](https://secure.travis-ci.org/freizl/samples-haskell-scotty.svg?branch=master)](https://travis-ci.com/freizl/samples-haskell-scotty)

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

- [ ] use `.well-known` endpoint to fetch authenticate url and issuer.
- [ ] can travis have both haskell and node?
- [ ] add stylish-haskell and hlint as dependencies
- [ ] sample app shall be able to take a file parameter for reading configs
- [ ] unit test is broken
- [ ] add nix-build (which is very fast in CI)
