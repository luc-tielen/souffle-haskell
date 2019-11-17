
build:
		@cabal build

configure:
		@hpack --force && cabal configure --enable-tests

lint:
		@hlint .

hoogle:
		hoogle server --local -p 8080

tests:
		cabal test

.PHONY: hoogle lint configure build tests
