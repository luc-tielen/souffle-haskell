{-# LANGUAGE GADTs, DataKinds #-}

module Language.Souffle.Experimental.Render
  ( renderIO
  , render
  ) where

import Control.Monad.Reader
import Language.Souffle.Experimental.Types
import Data.List.NonEmpty ( toList )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


data RenderMode = Nested | TopLevel

renderIO :: FilePath -> DL -> IO ()
renderIO path = TIO.writeFile path . render

render :: DL -> T.Text
render = flip runReader TopLevel . f where
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
    Fact name terms -> do
      let rendered = T.pack name <> "(" <> renderTerms (toList terms) <> ")"
      end <- maybeDot
      pure $ rendered <> end
    Relation name terms body -> do
      body' <- f body
      let rendered =
            T.pack name <> "(" <> renderTerms (toList terms) <> ") :-\n" <>
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
    Not e -> do
      -- TODO: refactor, should be handled in and?
      let maybeAddParens txt = case e of
            And _ _ -> "(" <> txt <> ")"
            _ -> txt
      txt <- maybeAddParens <$> local (const Nested) (f e)
      end <- maybeDot
      case end of
        "." -> pure $ "!" <> txt <> end
        _ -> pure $ "!" <> txt

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

renderTerms :: [SimpleTerm] -> T.Text
renderTerms = T.intercalate ", " . fmap renderTerm

renderTerm :: SimpleTerm -> T.Text
renderTerm = \case
  I x -> T.pack $ show x
  S s -> "\"" <> T.pack s <> "\""
  V v -> T.pack v
