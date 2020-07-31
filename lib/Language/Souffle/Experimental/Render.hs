{-# LANGUAGE GADTs, DataKinds #-}

module Language.Souffle.Experimental.Render
  ( render
  ) where

import Control.Monad.Reader
import Language.Souffle.Experimental.Types
import Data.List.NonEmpty ( toList )
import qualified Data.Text as T


data RenderMode = Nested | TopLevel

render :: DL ctx -> T.Text
render = flip runReader TopLevel . f where
  f :: DL ctx -> Reader RenderMode T.Text
  f = \case
    Program stmts -> do
      T.unlines <$> traverse f stmts
    TypeDef name dir ts ->
      let pairs = zip [1..] ts
          nameTypePairs = map (uncurry renderType) pairs
       in pure $ T.intercalate "\n"
        [ ".decl " <> T.pack name <> "(" <> T.intercalate ", " nameTypePairs <> ")"
        , renderDir name dir
        ]
    Fact name atoms -> do
      let rendered = T.pack name <> "(" <> renderAtoms (toList atoms) <> ")"
      end <- maybeDot
      pure $ rendered <> end
    Relation name atoms body -> do
      body' <- f body
      let rendered =
            T.pack name <> "(" <> renderAtoms (toList atoms) <> ") :-\n" <>
            T.intercalate "\n" (map indent $ T.lines body')
      pure rendered
    And e1 e2 -> do
      txt <- local (const Nested) $ do
        txt1 <- f e1
        txt2 <- f e2
        pure $ txt1 <> ",\n" <> txt2
      end <- maybeDot
      pure $ txt <> end
    Or e1 e2 -> do
      txt <- local (const Nested) $ do
        txt1 <- f e1
        txt2 <- f e2
        pure $ txt1 <> ";\n" <> txt2
      end <- maybeDot
      case end of
        "." -> pure $ txt <> end
        _ -> pure $ "(" <> txt <> ")"
    Not _ -> pure ""  -- TODO
  maybeDot = ask >>= \case
    TopLevel -> pure "."
    Nested -> pure mempty

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

renderAtoms :: [SimpleAtom] -> T.Text
renderAtoms = T.intercalate ", " . fmap renderAtom

renderAtom :: SimpleAtom -> T.Text
renderAtom = \case
  I x -> T.pack $ show x
  S s -> "\"" <> T.pack s <> "\""
  V v -> T.pack v
