## entr: http://eradman.com/entrproject/
##
watch:
	find samples buildtool -name '*.hs' | entr -s 'make build'

clean:
	cabal v2-clean

build:
	cabal v2-build all

test:
	cabal v2-test all

stylelish:
	find samples buildtool -name '*.hs' | xargs stylish-haskell -i

hlint:
	hlint . --report

hpack:
	hpack -f samples/login-with-okta
	hpack -f buildtool
