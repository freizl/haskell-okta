[![Build Status](https://travis-ci.com/freizl/haskell-okta.svg?branch=master)](https://travis-ci.com/freizl/haskell-okta)

# TODO

- [ ] make more UI interactive
  - allow input AS ID
  - table to show up how AT and IT has been verified and its result.
- [ ] fix unit test
- [ ] method for verify ID token and Access Token
  - how can have a global Reader/State to pull options/configs?
- [ ] explore `microlens-platform`.
  - one thing stops me is `makeClassyPrisms` in `JWT.hs`.
- [ ] logger
- [ ] reinvent https://oidcdebugger.com/ ?
- [X] unit test is broken
- [X] use `.well-known` endpoint to fetch authenticate url and issuer.
- [x] merge `custom-login` and `okta-hosted-login`
- [X] clear cookie that set by SIW
- [-] add stylish-haskell and hlint as dependencies
- [X] sample app shall be able to paramaters for run against different issuer
- [X] show ID token in web page
