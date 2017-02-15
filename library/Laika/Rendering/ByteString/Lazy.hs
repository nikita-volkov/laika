module Laika.Rendering.ByteString.Lazy
where

import Laika.Prelude
import qualified Laika.Building.Template as A
import qualified Laika.Rendering.ByteString.Builder as C
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as D


{-|
Renders the snippet into a lazy byte-array.

This is merely a convenience wrapper for the following:

@'B.toLazyByteString' . 'C.html'@
-}
{-# INLINE html #-}
html :: A.HTML -> D.ByteString
html =
  B.toLazyByteString . C.html
