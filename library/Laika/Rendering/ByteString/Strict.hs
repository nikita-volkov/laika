module Laika.Rendering.ByteString.Strict
where

import Laika.Prelude
import qualified Laika.Building.Template as A
import qualified Laika.Rendering.ByteString.Lazy as B
import qualified Data.ByteString.Lazy as C


{-|
Renders the snippet into a strict byte-array.

This is merely a convenience wrapper for the following:

@'C.toStrict' . 'B.html'@
-}
{-# INLINE html #-}
html :: A.HTML -> ByteString
html =
  C.toStrict . B.html
