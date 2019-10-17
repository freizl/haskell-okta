## entr: http://eradman.com/entrproject/
##
SRC=examples

watch:
	find $(SRC) -name '*.hs' | entr -s 'make build'

clean:
	cabal v2-clean

build:
	cabal v2-build all

test:
	cabal v2-test all

stylelish:
	find $(SRC) -name '*.hs' | xargs stylish-haskell -i

hlint:
	hlint . --report

hpack:
	hpack -f examples/login-with-okta

cabal2nix:
	cd examples/login-with-okta && cabal2nix . > ./login-with-okta.nix

####################
### CI
####################

ci-build: clean
	nix-build

ci-lint:
	nix-shell --command 'make hlint'
