#!/usr/bin/env runhaskell

-- NOTE: This is a helper script for importing all Souffle headers into
-- this repository. This is done in order to make Haskell libraries that
-- use souffle-haskell "self-contained", meaning users of the packages
-- using those libraries are not required to have souffle (headers) installed.

{-# LANGUAGE ViewPatterns #-}

import System.Directory
import System.Process
import Control.Monad
import Data.List
import Data.Maybe


run :: String -> IO ()
run = callCommand

runWithResult :: String -> IO String
runWithResult (words -> (program:args)) = readProcess program args ""

headerDir :: FilePath
headerDir = "cbits/souffle/"

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  gitRoot <- getGitRootDirectory
  when (pwd /= gitRoot) $ do
    putStrLn "You need to run this script in the root directory of this repo! Aborting."

  run "git submodule update --init --recursive"
  run $ "cp souffle/LICENSE " <> headerDir
  files <- copyHeaders
  putStrLn "Replace 'install-includes' in your package.yaml with the following:"
  putStrLn $ unlines $ map ("  - " <>) files

getGitRootDirectory :: IO FilePath
getGitRootDirectory =
  filter (/= '\n') <$> runWithResult "git rev-parse --show-toplevel"

copyHeaders :: IO [FilePath]
copyHeaders = do
  let command = unwords
        [ "find souffle -type f -name \"*.h\" |"
        , "xargs tar cf - | "
        , "(cd " <> headerDir <> "; tar xf - --strip-components=2)"
        ]
  run command
  headers <- lines <$> runWithResult ("find " <> headerDir <> " -type f")
  pure $ mapMaybe (stripPrefix headerDir) headers

