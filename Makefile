## entr: http://eradman.com/entrproject/
##
SRC=examples

## replace by https://github.com/ndmitchell/ghcid
watch:
	find $(SRC) -name '*.hs' | entr -s 'make build'

clean:
	cabal v2-clean

build: hpack
	cabal v2-build all

test:
	cabal v2-test all

stylelish:
	find $(SRC) -name '*.hs' | xargs stylish-haskell -i

hlint: stylelish
	hlint . --report

hpack:
	hpack -f examples/login-with-okta

## TODO: disable test in nix-build due to version conflict of wai-extra
cabal2nix:
	cd examples/login-with-okta && cabal2nix --no-check . > ./login-with-okta.nix

####################
### CI
####################

## there is build error when using lens-4.17.1 at JWT.hs (line 36 `makeClassyPrisms ''OktaJWTError`)
## hence override the deps (lens, jose)
ci-build: clean
	nix-build --attr login-with-okta

ci-hlint:
	nix-shell --command 'make hlint'
