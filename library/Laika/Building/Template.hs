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
newtype Snippet =
  Snippet A.Builder

deriving instance Monoid Snippet

instance Semigroup Snippet

instance IsString Snippet where
  {-# INLINE fromString #-}
  fromString =
    text . fromString

{-|
Turns text into Snippet,
applying the UTF-8 and HTML-entity encodings to it.
-}
{-# INLINE text #-}
text :: Text -> Snippet
text text =
  Snippet (B.textInUTF8HTML text)

