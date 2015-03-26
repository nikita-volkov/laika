module Laika 
(
  file,
)
where

import Laika.Prelude
import Language.Haskell.TH
import qualified Record.Types
import qualified Record.Lens
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Laika.Parser as Parser


file :: FilePath -> Q Exp
file =
  join . fmap (either fail return) .
  fmap (join . fmap (renderingLambda [Arg])) .
  runIO . Parser.run . 
  join . fmap (const (Parser.parseTemplate <* Parser.endOfInput)) .
  Parser.load


type Path =
  [PathSegment]

data PathSegment =
  Arg | Field Text
  deriving (Eq, Show)

resolvePath :: Path -> Parser.Path -> Either String Path
resolvePath p pp =
  ($ view [l|segments|] pp) $
  ($ if view [l|absolute|] pp then [Arg] else p ) $
  foldM $ \p -> \case
    Parser.Dot -> return p
    Parser.DoubleDot -> case p of
      h:t -> return t
      _ -> Left $ "Attempt to step out a level from an empty path"
    Parser.Identifier n -> return $ Field n : p

renderingLambda :: Path -> Parser.Template -> Either String Exp
renderingLambda path =
  fmap (LamE [VarP (mkName ("x" <> show depth))] .
        AppE (VarE 'mconcat) .
        ListE) .
  traverse phraseExp
  where
    depth =
      length $ filter (== Arg) $ path
    phraseExp =
      \case
        Parser.Reference r ->
          do
            path' <- resolvePath path (view [l|path|] r)
            return $
              bool id (AppE (AppE (VarE 'mapLazyText) (VarE 'escapeHTML))) (view [l|escaped|] r) $
              pathValue path'
        Parser.Block r ->
          do
            path' <- resolvePath path (view [l|path|] r)
            lambda <- renderingLambda (Arg : path') (view [l|template|] r)
            return $ AppE (AppE (VarE 'foldMap) lambda) (pathValue path')
        Parser.Text t ->
          pure $
          AppE (VarE 'TLB.fromText) $ 
          LitE (StringL (T.unpack t))

pathValue :: Path -> Exp
pathValue path =
  foldr (\l r -> UInfixE l (VarE '($)) r) (VarE (mkName ("x" <> show argDepth))) .
  map (AppE (VarE 'Record.Lens.view) .
       AppE (VarE 'fieldSimpleLens) . 
       SigE (VarE 'undefined) . 
       AppT (ConT ''Record.Types.Field) . LitT . StrTyLit . T.unpack) .
  foldMap (\case Field n -> pure n; _ -> empty) .
  takeWhile (/= Arg) 
  $ path
  where
    argDepth =
      length $ filter (== Arg) $ path

purifyQ :: Q a -> a
purifyQ = unsafePerformIO . runQ

fieldSimpleLens :: Record.Types.Field n s s a a => 
                   Record.Types.FieldName n -> Record.Lens.Lens s s a a
fieldSimpleLens = 
  Record.Types.fieldLens

mapLazyText :: (TL.Text -> TL.Text) -> TLB.Builder -> TLB.Builder
mapLazyText f =
  TLB.fromLazyText . f . TLB.toLazyText

escapeHTML :: TL.Text -> TL.Text
escapeHTML = 
  TL.concatMap $ \case
    '&'  -> "&amp;"
    '\\' -> "&#92;"
    '"'  -> "&quot;"
    '\'' -> "&#39;"
    '<'  -> "&lt;"
    '>'  -> "&gt;"
    h    -> TL.singleton h
