
# NOTE: these all assume cabal v3

build: configure
		@cabal build

configure:
		@hpack --force && cabal configure --enable-tests

clean:
		@cabal clean

lint:
		@hlint .

hoogle:
		hoogle server --local -p 8080

tests: configure
		DATALOG_DIR=tests/fixtures/ cabal run souffle-haskell-test

docs:
		@cabal haddock

bench:
		@cabal run souffle-haskell-benchmarks -- --output /tmp/benchmarks.html

.PHONY: hoogle lint clean configure build tests docs bench
