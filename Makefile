## entr: http://eradman.com/entrproject/
##
watch:
	find samples buildtool | entr -s 'make build'

build:
	cabal v2-build all

test:
	cabal v2-test all
