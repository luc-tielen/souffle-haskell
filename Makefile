
# NOTE: these all assume cabal v3

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
		DATALOG_DIR=tests/fixtures/ cabal run souffle-haskell-test

docs:
	@cabal haddock

.PHONY: hoogle lint clean configure build tests docs
