
# Souffle-haskell

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/luc-tielen/souffle-haskell/blob/master/LICENSE)
[![Hackage](https://img.shields.io/hackage/v/souffle-haskell?style=flat-square)](https://hackage.haskell.org/package/souffle-haskell)

This repo provides Haskell bindings for performing analyses with the
[Souffle Datalog language](https://github.com/souffle-lang/souffle).
It does this by binding directly to an "embedded" Souffle program
(previously generated with `souffle -g`).

Fun fact: this library combines both functional programming (Haskell),
logic programming (Datalog / Souffle) and imperative / OO programming (C / C++).


## Motivating example

Let's first write a datalog program that can check if 1 point
is reachable from another:

```prolog
// We define 2 data types:
.decl edge(n: symbol, m: symbol)
.decl reachable(n: symbol, m: symbol)

// We indicate we are interested in "reachable" facts.
// NOTE: If you forget to add outputs, the souffle compiler will
//       try to be smart and remove most generated code!
.output reachable

// We write down some pre-defined facts on the datalog side.
edge("a", "b").
edge("b", "c").
edge("c", "e").
edge("e", "f").
edge("c", "d").

// And we tell datalog how to check if 1 point is reachable from another.
reachable(x, y) :- edge(x, y).                  // base rule
reachable(x, z) :- edge(x, y), reachable(y, z). // inductive rule
```

Now that we have the datalog code, we can generate a `path.cpp` from it
using `souffle -g path.cpp path.dl`. `souffle-haskell` can bind to this program
in the following way:

```haskell
-- Enable some necessary extensions:
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DataKinds, TypeFamilies, DeriveGeneric #-}

module Main ( main ) where

import Data.Foldable ( traverse_ )
import Control.Monad.IO.Class
import GHC.Generics
import Data.Vector
import qualified Language.Souffle.TH as Souffle
import qualified Language.Souffle as Souffle

-- We only use template haskell for directly embedding the .cpp file into this file.
-- If we do not do this, it will link incorrectly due to the way the
-- C++ code is generated.
Souffle.embedProgram "/path/to/path.cpp"


-- We define a data type representing our datalog program.
data Path = Path

-- Facts are represent in Haskell as simple product types,
-- Numbers map to Int32, symbols to Strings / Text.

data Edge = Edge String String
  deriving (Eq, Show, Generic)

data Reachable = Reachable String String
  deriving (Eq, Show, Generic)

-- By making Path an instance of Program, we provide Haskell with information
-- about the datalog program. It uses this to perform compile-time checks to
-- limit the amount of possible programmer errors to a minimum.
instance Souffle.Program Path where
  type ProgramFacts Path = [Edge, Reachable]
  programName = const "path"

-- By making a data type an instance of Edge, we give Haskell the
-- necessary information to bind to the datalog fact.
instance Souffle.Fact Edge where
  factName = const "edge"

instance Souffle.Fact Reachable where
  factName = const "reachable"

-- For simple product types, we can automatically generate the
-- marshalling/unmarshalling code of data between Haskell and datalog.
instance Souffle.Marshal Edge
instance Souffle.Marshal Reachable


main :: IO ()
main = Souffle.runSouffle $ do
  maybeProgram <- Souffle.init Path  -- Initializes the Souffle program.
  case maybeProgram of
    Nothing -> liftIO $ putStrLn "Failed to load program."
    Just prog -> do
      Souffle.addFact prog $ Edge "d" "i"   -- Adding a single fact from Haskell side
      Souffle.addFacts prog [ Edge "e" "f"  -- Adding multiple facts
                            , Edge "f" "g"
                            , Edge "f" "g"
                            , Edge "f" "h"
                            , Edge "g" "i"
                            ]

      Souffle.run prog  -- Run the Souffle program

      -- NOTE: You can change type param to fetch different relations
      --       Here it requires an annotation since we directly print it
      --       to stdout, but if passed to another function, it can infer
      --       the correct type automatically.
      --       A list of facts can also be returned here.
      results :: Vector Reachable <- Souffle.getFacts prog
      liftIO $ traverse_ print results

      -- We can also look for a specific fact:
      maybeFact <- Souffle.findFact prog $ Reachable "a" "c"
      liftIO $ print $ maybeFact
```

For more examples of how to use the top level API, you can also take a look at
the tests.


## Getting started

This library assumes that the Souffle include paths are properly set.
This is needed in order for the C++ code to be compiled correctly.
The easiest way to do this (that I know of) is via [Nix](https://nixos.org/nix/).
Add `souffle` to the build inputs of your derivation and everything will
be set correctly.
Without Nix, you will have to follow the manual install instructions
on the [Souffle website](https://souffle-lang.github.io/install).

In your package.yaml / *.cabal file, make sure to add the following options
(assuming package.yaml here):

```yaml
# ...
cpp-options:
  - -D__EMBEDDED_SOUFFLE__

# ...
```

This will instruct the Souffle compiler to compile the C++ in such a way that
it can be linked with other languages (including Haskell!).


## Contributing

TLDR: Nix-based project; the Makefile contains the most commonly used commands.


Long version:

The project makes use of [Nix](https://nixos.org/nix/download.html) to setup the development environment.
Setup your environment by entering the following command:

```bash
$ nix-shell
```

After this command, you can build the project:

```bash
$ make configure  # configures the project
$ make build      # builds the haskell code
$ make lint       # runs the linter
$ make hoogle     # starts a local hoogle webserver
```


## Issues

Found an issue or missing a piece of functionality?
Please open an issue with a description of the problem.

