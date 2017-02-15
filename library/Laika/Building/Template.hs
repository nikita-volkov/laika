module Laika.Building.Template where

import Laika.Prelude
import qualified Data.ByteString.Builder as A
import qualified Laika.Building.ByteString.Builder as B


{-|
A snippet of HTML.
Can be efficiently concatenated with other snippets,
using such functions as 'Data.Semigroup.<>' and 'mappend'.

Can be rendered into plentiful data-types,
which are provided by the Rendering modules of the library.
-}
newtype HTML =
  HTML A.Builder

deriving instance Monoid HTML

instance Semigroup HTML

instance IsString HTML where
  {-# INLINE fromString #-}
  fromString =
    text . fromString

{-|
Turns text into HTML,
applying the UTF-8 and HTML-entity encodings to it.
-}
{-# INLINE text #-}
text :: Text -> HTML
text text =
  HTML (B.textInUTF8HTML text)

