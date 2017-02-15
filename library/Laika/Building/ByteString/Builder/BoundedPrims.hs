module Laika.Building.ByteString.Builder.BoundedPrims where

import Laika.Prelude
import Data.ByteString.Builder.Prim
import qualified Laika.Building.ByteString.Builder.FixedPrims as A


{-# INLINE fixed #-}
fixed :: FixedPrim a -> BoundedPrim a
fixed =
  liftFixedToBounded

{-# INLINE html #-}
html :: BoundedPrim Word8
html =
  greaterThanOr $ lesserThanOr $ ampersandOr $ doubleQuoteOr $ fixed word8

{-# INLINE betweenTagsHTML #-}
betweenTagsHTML :: BoundedPrim Word8
betweenTagsHTML =
  lesserThanOr $ ampersandOr $ fixed word8

{-# INLINE betweenDoubleQuotes #-}
betweenDoubleQuotes :: BoundedPrim Word8
betweenDoubleQuotes =
  doubleQuoteOr $ ampersandOr $ fixed word8

{-# INLINE inTagHTML #-}
inTagHTML :: BoundedPrim Word8
inTagHTML =
  greaterThanOr $ ampersandOr $ fixed word8

{-# INLINE greaterThanOr #-}
greaterThanOr :: BoundedPrim Word8 -> BoundedPrim Word8
greaterThanOr =
  condB (== 62) (fixed A.greaterThan)

{-# INLINE lesserThanOr #-}
lesserThanOr :: BoundedPrim Word8 -> BoundedPrim Word8
lesserThanOr =
  condB (== 60) (fixed A.lesserThan)

{-# INLINE ampersandOr #-}
ampersandOr :: BoundedPrim Word8 -> BoundedPrim Word8
ampersandOr =
  condB (== 38) (fixed A.ampersand)

{-# INLINE doubleQuoteOr #-}
doubleQuoteOr :: BoundedPrim Word8 -> BoundedPrim Word8
doubleQuoteOr =
  condB (== 34) (fixed A.doubleQuote)

{-# INLINE singleQuoteOr #-}
singleQuoteOr :: BoundedPrim Word8 -> BoundedPrim Word8
singleQuoteOr =
  condB (== 39) (fixed A.singleQuote)
