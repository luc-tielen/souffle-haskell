{-# LANGUAGE DataKinds, TypeFamilies, DeriveGeneric, DeriveAnyClass #-}

module Main ( main ) where

-- NOTE: This is a helper script for importing all Souffle headers into
-- this repository. This is done in order to make Haskell libraries that
-- use souffle-haskell "self-contained", meaning users of the packages
-- using those libraries are not required to have souffle (headers) installed.

import System.Directory
import System.FilePath
import System.Process
import Control.Monad
import Data.List
import Data.Maybe
import Data.Void
import GHC.Generics
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Language.Souffle.Interpreted as Souffle


data Includes = Includes FilePath FilePath
  deriving (Eq, Show, Generic, Souffle.Marshal)

newtype TopLevelInclude = TopLevelInclude FilePath
  deriving (Eq, Show, Generic, Souffle.Marshal)

newtype RequiredInclude = RequiredInclude FilePath
  deriving (Eq, Show, Generic, Souffle.Marshal)

data Handle = Handle

instance Souffle.Program Handle where
  type ProgramFacts Handle = [TopLevelInclude, Includes, RequiredInclude]
  programName = const "required_include"

instance Souffle.Fact Includes where
  factName = const "includes"

instance Souffle.Fact TopLevelInclude where
  factName = const "top_level_include"

instance Souffle.Fact RequiredInclude where
  factName = const "required_include"


run :: String -> IO ()
run = callCommand

runWithResult :: String -> IO String
runWithResult s = case words s of
  (program:args) -> readProcess program args ""
  _ -> error "Passed empty string to 'runWithResult'. Aborting."

headerDir :: FilePath
headerDir = "cbits/souffle/"

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  gitRoot <- getGitRootDirectory
  when (pwd /= gitRoot) $
    putStrLn "You need to run this script in the root directory of this repo! Aborting."

  run "git submodule update --init --recursive"
  run $ "rm -rf " <> headerDir <> "*.h"
  run $ "cp souffle/LICENSE " <> headerDir
  files <- copyHeaders
  putStrLn "Replace 'install-includes' in your package.yaml with the following:"
  putStrLn . unlines $ map ("  - " <>) files

getGitRootDirectory :: IO FilePath
getGitRootDirectory =
  filter (/= '\n') <$> runWithResult "git rev-parse --show-toplevel"

parseInclude :: String -> Maybe FilePath
parseInclude s = either (const Nothing) Just (P.runParser parser "" s) where
  parser :: P.Parsec Void String FilePath
  parser = takeFileName <$> do
    P.chunk "#include" *> P.space1
    P.between quotes quotes $
      P.takeWhile1P Nothing (\c -> c /= ' ' && c /= '"')
  quotes = P.char '"'

parseIncludes :: FilePath -> IO [Includes]
parseIncludes file =
  let file' = takeFileName file
  in map (file' `Includes`) . mapMaybe parseInclude . lines <$> readFile file

copyHeaders :: IO [FilePath]
copyHeaders = do
  headers <- filter (".h" `isSuffixOf`) . lines
          <$> runWithResult "find souffle -type f"
  includes <- concat <$> traverse parseIncludes headers
  cfg <- Souffle.defaultConfig
  let config = cfg { Souffle.cfgDatalogDir = "./scripts" }
  requiredIncludes <- Souffle.runSouffleWith config $
    Souffle.init Handle >>= \case
      Nothing -> error "Failed to load Souffle program. Aborting."
      Just prog -> do
        Souffle.addFacts prog [ TopLevelInclude "SouffleInterface.h"
                              , TopLevelInclude "CompiledSouffle.h"
                              ]
        Souffle.addFacts prog includes
        Souffle.run prog
        Souffle.getFacts prog
  let requiredIncludes' = map (\(RequiredInclude include) -> include) requiredIncludes
  forM requiredIncludes' copyHeader

copyHeader :: FilePath -> IO FilePath
copyHeader file = do
  header <- head . filter (("/" </> file) `isSuffixOf`) . lines
        <$> runWithResult "find souffle/ -type f"
  let header' = joinPath $ drop 2 $ splitPath header
      dir = headerDir </> takeDirectory header'
      destination = replaceDirectory header' dir
  createDirectoryIfMissing True dir
  copyFile header destination
  pure header'

