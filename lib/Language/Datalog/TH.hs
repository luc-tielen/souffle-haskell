
module Language.Datalog.TH ( embedSouffleProgram ) where

import Language.Haskell.TH.Syntax


embedSouffleProgram :: String -> Q [Dec]
embedSouffleProgram path = [] <$ qAddForeignFilePath LangCxx path
