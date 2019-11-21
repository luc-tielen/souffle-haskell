
module Language.Souffle.TH ( embedProgram ) where

import Language.Haskell.TH.Syntax


embedProgram :: String -> Q [Dec]
embedProgram path = [] <$ qAddForeignFilePath LangCxx path
