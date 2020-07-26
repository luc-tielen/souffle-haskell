# Souffle-haskell

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/luc-tielen/souffle-haskell/blob/master/LICENSE)
[![CircleCI](https://circleci.com/gh/luc-tielen/souffle-haskell.svg?style=svg&circle-token=07fcf633c70820100c529dda8869baa60d4b6dd8)](https://circleci.com/gh/luc-tielen/souffle-haskell)
[![Hackage](https://img.shields.io/hackage/v/souffle-haskell?style=flat-square)](https://hackage.haskell.org/package/souffle-haskell)

This repo provides Haskell bindings for performing analyses with the
[Souffle Datalog language](https://github.com/souffle-lang/souffle).

Fun fact: this library combines both functional programming (Haskell),
logic programming (Datalog / Souffle) and imperative / OO programming (C / C++).

## Motivating example

Let's first write a datalog program that can check if one point
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
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, DeriveGeneric #-}

module Main ( main ) where

import Data.Foldable ( traverse_ )
import Control.Monad.IO.Class
import GHC.Generics
import Data.Vector
import qualified Language.Souffle.Compiled as Souffle


-- We define a data type representing our datalog program.
data Path = Path

-- Facts are represented in Haskell as simple product types,
-- Numbers map to Int32, unsigned to Word32, floats to Float,
-- symbols to Strings / Text.

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

-- By making a data type an instance of Fact, we give Haskell the
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

In your package.yaml or .cabal file, make sure to add the following options
(assuming package.yaml here):

```yaml
# ...

cxx-options:
  - -D__EMBEDDED_SOUFFLE__
cxx-sources:
  - /path/to/FILE.cpp # be sure to change this according to what you need!

# ...
```

This will instruct the Souffle compiler to compile the C++ in such a way that
it can be linked with other languages (including Haskell!).

For an example, take a look at the configuration for the
[test suite](https://github.com/luc-tielen/souffle-haskell/blob/master/package.yaml#L68-L80) of this project.

If you run into C++ compilation issues when using stack, this might be because
the `-std=c++17` flag is not being used correctly when compiling souffle-haskell.
To fix this, you can add the following to your `stack.yaml`:

```yaml
ghc-options:
  souffle-haskell: -optcxx-std=c++17
```

## Documentation

The documentation for the library can be found on
[Hackage](https://hackage.haskell.org/package/souffle-haskell).
`Language.Souffle.Class` is a good starting point for getting an overview of
the top level API.

## Supported modes

Souffle programs can be run in 2 ways. They can either run in **interpreted** mode
(using the `souffle` CLI command), or they can be **compiled** to C++-code and
called from a host program for improved efficiency. This library supports both
modes (since version 0.2.0). The two variants have only a few minor differences
and can be swapped fairly easily.

### Interpreted mode

This is probably the mode you want to start out with if you are developing a
program that uses Datalog for computing certain relations. Interpreted mode
offers quick development iterations (no compiling of C++ code each time you
change your Datalog code). However because the Souffle code is interpreted,
it can't offer the same speed as in compiled mode.

The main differences with compiled mode are the following:

1. You need to import `Language.Souffle.Interpreted`
2. You need to call `Souffle.cleanup` after you no longer need the Souffle
   functionality. This will clean up the generated CSV fact files located in
   a temporary directory.

#### Interpreter configuration

The interpreter uses CSV files to read or write facts. The configuration
allows specifiying where the fact directory is located. With the default
configuration, it will try to lookup `DATALOG_DIR` in the environment and
fall back to the current directory (or `.`).

You can also configure which souffle executable will be used. By default,
it will first look at the `SOUFFLE_BIN` environment variable. If this is
not set, it will try to find the executable using the `which` shell-command.
If it also can't find the executable this way, then it will fail to
initialize the interpreter.

For more information regarding configuration, take a look at the
`runSouffleWith` function.

The separators in the CSV fact files cannot be configured at the moment.
A tab character (`'\t'`) is used to separate the different columns.

### Compiled mode

Once the prototyping phase of the Datalog algorithm is over, it is advised
to switch over to the compiled mode. It offers much improved performance
compared to the interpreted mode, at the cost of having to recompile your
Datalog algorithm each time it changes.

The main differences with interpreted mode are the following:

1. Compile the Datalog code with `souffle -g`.
2. Remove `Souffle.cleanup` if it is present in your code, compiled mode
   leaves no CSV artifacts.

The [motivating example](#motivating-example) is a complete example for the compiled mode.

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
