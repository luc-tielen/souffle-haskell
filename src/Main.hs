
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DataKinds, TypeFamilies, DeriveGeneric #-}

module Main ( main ) where

import Prelude hiding ( init )
import Data.Foldable ( traverse_ )
import Control.Monad.IO.Class
import GHC.Generics
import Language.Souffle.TH
import Language.Souffle

embedProgram "path.cpp"


data Path = Path

data Edge = Edge String String
  deriving (Eq, Show, Generic)

data Reachable = Reachable String String
  deriving (Eq, Show, Generic)

instance Program Path where
  type ProgramFacts Path = [Edge, Reachable]
  programName = const "path"

instance Fact Edge where
  factName = const "edge"

instance Fact Reachable where
  factName = const "reachable"

instance Marshal Edge
instance Marshal Reachable


main :: IO ()
main = runSouffle $ do
  maybeProgram <- init Path
  case maybeProgram of
    Nothing -> liftIO $ putStrLn "Failed to load program."
    Just prog -> do
      addFact prog $ Edge "d" "i"
      addFacts prog [ Edge "e" "f"
                    , Edge "f" "g"
                    , Edge "f" "g"
                    , Edge "f" "h"
                    , Edge "g" "i"
                    ]
      run prog
      -- NOTE: change type param to fetch different relations
      results :: [Reachable] <- getFacts prog
      liftIO $ traverse_ print results

