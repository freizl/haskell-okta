## entr: http://eradman.com/entrproject/
##
watch:
	find samples buildtool | entr -s 'cabal v2-build all'
