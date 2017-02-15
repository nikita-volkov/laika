module Laika.Building.SnippetExp where

import Laika.Prelude
import Language.Haskell.TH
import qualified Data.ByteString.Builder as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as G
import qualified Data.Text as C
import qualified Data.Text.Encoding as F
import qualified Laika.Building.ByteString.Builder as E
import qualified Laika.Building.Template as H


data Builder =
  Bytes [Word8] |
  NodePlaceholder Text |
  AttrPlaceholder Text |
  Concatenation Builder Builder

instance Monoid Builder where
  mempty =
    Bytes []
  mappend =
    \case
      Bytes bytes1 -> \case
        Bytes bytes2 -> Bytes (bytes1 <> bytes2)
        Concatenation (Bytes bytes2) builder3 -> mappend (Bytes (bytes1 <> bytes2)) builder3
        builder2 -> Concatenation (Bytes bytes1) builder2
      Concatenation builder1 builder2 -> Concatenation builder1 . mappend builder2
      builder1 -> \case
        Concatenation builder2 builder3 -> Concatenation (mappend builder1 builder2) builder3
        builder2 -> Concatenation builder1 builder2

instance Semigroup Builder

instance IsString Builder where
  fromString =
    unescapedText . fromString

deriving instance Show Builder

run :: Builder -> Exp
run x =
  case x of
    Bytes [] ->
      VarE 'mempty
    Bytes list ->
      AppE (ConE 'H.Snippet) (AppE (AppE (VarE 'E.byteListOfLength) lengthE) byteListE)
      where
        byteListE =
          ListE (map (LitE . IntegerL . fromIntegral) list)
        lengthE =
          LitE (IntegerL (fromIntegral (length list)))
    Concatenation left right ->
      AppE (AppE (VarE 'mappend) (run left)) (run right)
    NodePlaceholder name ->
      VarE (mkName (C.unpack name))
    AttrPlaceholder name ->
      AppE (VarE 'H.text) (VarE (mkName (C.unpack name)))

bytes :: [Word8] -> Builder
bytes =
  Bytes

bytesBuilder :: A.Builder -> Builder
bytesBuilder =
  bytes . G.unpack . A.toLazyByteString

unescapedText :: Text -> Builder
unescapedText =
  bytesBuilder . F.encodeUtf8Builder

escapedText :: Text -> Builder
escapedText =
  bytesBuilder . E.textInUTF8HTML

nodePlaceholder :: Text -> Builder
nodePlaceholder =
  NodePlaceholder

attrPlaceholder :: Text -> Builder
attrPlaceholder =
  AttrPlaceholder
