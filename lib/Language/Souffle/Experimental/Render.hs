module Language.Souffle.Experimental.Render
  ( render
  ) where

import Language.Souffle.Experimental.Types
import Data.List.NonEmpty ( toList )
import qualified Data.Text as T


render :: DL -> T.Text
render = \case
  Program stmts ->
    T.unlines $ fmap render stmts
  TypeDef name dir ts ->
    let pairs = zip [1..] ts
        nameTypePairs = map (uncurry renderType) pairs
    in T.intercalate "\n"
      [ ".decl " <> T.pack name <> "(" <> T.intercalate ", " nameTypePairs <> ")"
      , renderDir name dir
      ]
  Fact name atoms ->
    T.pack name <> "(" <> renderAtoms (toList atoms) <> ")."
  Relation name atoms body ->
    T.pack name <> "(" <> renderAtoms (toList atoms) <> ") :-\n" <>
      T.intercalate "\n" (map indent $ T.lines (render body))
  And _ _ -> ""
  Or _ _ -> ""
  Not _ -> ""

indent :: T.Text -> T.Text
indent = ("  " <>)

renderDir :: VarName -> Direction -> T.Text
renderDir name = \case
  In -> ".input " <> T.pack name
  Out -> ".output " <> T.pack name
  InOut -> T.intercalate "\n" [renderDir name In, renderDir name Out]

renderType :: Int -> DLType -> T.Text
renderType x = \case
  DLInt -> x' <> ": number"
  DLString -> x' <> ": symbol"
  where x' = "t" <> T.pack (show x)

renderAtoms :: [Atom] -> T.Text
renderAtoms = T.intercalate ", " . fmap renderAtom

renderAtom :: Atom -> T.Text
renderAtom = \case
  Int x -> T.pack $ show x
  Str s -> "\"" <> T.pack s <> "\""
  Var v -> T.pack v
