
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DataKinds, TypeFamilies, DeriveGeneric #-}

module Main ( main ) where

import Data.Foldable ( traverse_ )
import Control.Monad.IO.Class
import GHC.Generics
import qualified Language.Souffle.TH as Souffle
import qualified Language.Souffle as Souffle

Souffle.embedProgram "path.cpp"


data Path = Path

data Edge = Edge String String
  deriving (Eq, Show, Generic)

data Reachable = Reachable String String
  deriving (Eq, Show, Generic)

instance Souffle.Program Path where
  type ProgramFacts Path = [Edge, Reachable]
  programName = const "path"

instance Souffle.Fact Edge where
  factName = const "edge"

instance Souffle.Fact Reachable where
  factName = const "reachable"

instance Souffle.Marshal Edge
instance Souffle.Marshal Reachable


main :: IO ()
main = Souffle.runSouffle $ do
  maybeProgram <- Souffle.init Path
  case maybeProgram of
    Nothing -> liftIO $ putStrLn "Failed to load program."
    Just prog -> do
      Souffle.addFact prog $ Edge "d" "i"
      Souffle.addFacts prog [ Edge "e" "f"
                    , Edge "f" "g"
                    , Edge "f" "g"
                    , Edge "f" "h"
                    , Edge "g" "i"
                    ]
      Souffle.run prog
      -- NOTE: change type param to fetch different relations
      results :: [Reachable] <- Souffle.getFacts prog
      liftIO $ traverse_ print results

