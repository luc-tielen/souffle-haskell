
-- | Helper module for easier integration of generated embedded souffle programs
--   with Haskell code. Without these helper functions, it becomes much harder
--   to link the C++ code directly to Haskell due to the way how Souffle
--   generates the code.
module Language.Souffle.TH ( embedProgram ) where

import Language.Haskell.TH.Syntax


-- | Helper function for embedding a Souffle program in Haskell.
--   Requires the use of the TemplateHaskell language extension.
--
--   The passed in String should be a path relative from the root of the
--   project where the .cpp file is located.
--
--   Example usage:
--
-- @
-- module Main where
-- import Language.Haskell.TH.Syntax as Souffle
-- Souffle.embedProgram "path\/to\/file.cpp"  -- NOTE: call directly on top level!
-- @
embedProgram :: String -> Q [Dec]
embedProgram path = [] <$ qAddForeignFilePath LangCxx path
