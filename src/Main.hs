
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Main ( main ) where

import Prelude hiding ( init )
import Data.Foldable ( traverse_ )
import Language.Souffle.TH
import Language.Souffle

embedProgram "path.cpp"


data Edge = Edge String String
  deriving (Eq, Show)

data Reachable = Reachable String String
  deriving (Eq, Show)

instance Fact Edge where
  push (Edge x y) = do
    push x
    push y
  pop = Edge <$> pop <*> pop

instance Fact Reachable where
  push (Reachable x y) = do
    push x
    push y
  pop = Reachable <$> pop <*> pop


main :: IO ()
main = do
  maybeProgram <- init "path"
  case maybeProgram of
    Nothing -> putStrLn "Failed to load program."
    Just prog -> do
      addFact prog "edge" $ Edge "d" "some_other_node"
      addFacts prog "edge" [ Edge "e" "f"
                           , Edge "f" "g"
                           , Edge "f" "h"
                           , Edge "g" "some_other_node"
                           ]
      run prog
      results :: [Reachable] <- getFacts prog "reachable"
      traverse_ print results

