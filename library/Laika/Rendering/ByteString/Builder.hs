module Laika.Rendering.ByteString.Builder
where

import Laika.Prelude
import qualified Laika.Building.Template as A
import qualified Data.ByteString.Builder as B


{-|
Renders the snippet into a byte-array builder.
-}
{-# INLINE html #-}
html :: A.Snippet -> B.Builder
html (A.Snippet builder) =
  builder
