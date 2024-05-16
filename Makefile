# NOTE: these all assume cabal v3

build: configure
		@cabal build

configure: ## Configure the project
		@cabal configure --enable-tests --enable-benchmarks

clean: ## Clean up the build artifacts
		@cabal clean

lint: ## Run the code linter (HLint)
	@find tests lib benchmark -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code styler (and cabal-fmt)
	@cabal-fmt -i *.cabal

hoogle: ## Start a hoogle server on port 8080
		hoogle server --local -p 8080

tests: configure ## Run tests
		DATALOG_DIR=tests/fixtures/ cabal run souffle-haskell-test

docs: ## Generate the documentation
		@cabal haddock

bench: ## Run the benchmarks
		@cabal run souffle-haskell-benchmarks -- --output /tmp/benchmarks.html

help: ## Display this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
