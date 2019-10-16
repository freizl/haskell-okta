## entr: http://eradman.com/entrproject/
##
watch:
	find samples buildtool -name '*.hs' | entr -s 'make build'

build:
	cabal v2-build all

test:
	cabal v2-test all

lint:
	cabal v2-exec buildtool -- stylish && cabal v2-exec buildtool -- lint
