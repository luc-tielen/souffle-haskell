
build:
		@cabal build

configure:
		@hpack --force && cabal configure --enable-tests

clean:
		@cabal clean

lint:
		@hlint .

hoogle:
		hoogle server --local -p 8080

tests:
		cabal test

.PHONY: hoogle lint clean configure build tests
