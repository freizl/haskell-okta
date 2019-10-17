[![Build Status](https://travis-ci.com/freizl/haskell-okta.svg?branch=master)](https://travis-ci.com/freizl/haskell-okta)

# TODO

- [X] unit test is broken
- [X] use `.well-known` endpoint to fetch authenticate url and issuer.
- [x] merge `custom-login` and `okta-hosted-login`
- [ ] fix unit test
- [ ] logger
- [ ] method for verify ID token and Access Token
  - how can have a global Reader/State to pull options/configs?
- [X] clear cookie that set by SIW
- [ ] add stylish-haskell and hlint as dependencies
- [X] sample app shall be able to paramaters for run against different issuer
- [X] show ID token in web page
