module Laika.Building.ByteString.Builder.FixedPrims where

import Laika.Prelude
import Data.ByteString.Builder.Prim


{-# INLINE lesserThan #-}
lesserThan :: FixedPrim a
lesserThan =
  fourStaticASCIIChars '&' 'l' 't' ';'

{-# INLINE greaterThan #-}
greaterThan :: FixedPrim a
greaterThan =
  fourStaticASCIIChars '&' 'g' 't' ';'

{-# INLINE ampersand #-}
ampersand :: FixedPrim a
ampersand =
  fiveStaticASCIIChars '&' 'a' 'm' 'p' ';'

{-# INLINE doubleQuote #-}
doubleQuote :: FixedPrim a
doubleQuote =
  fiveStaticASCIIChars '&' '#' '3' '4' ';'

{-# INLINE singleQuote #-}
singleQuote :: FixedPrim a
singleQuote =
  fiveStaticASCIIChars '&' '#' '3' '9' ';'

{-# INLINE fourStaticASCIIChars #-}
fourStaticASCIIChars :: Char -> Char -> Char -> Char -> FixedPrim a
fourStaticASCIIChars a b c d =
  const (a, (b, (c, d))) >$< char7 >*< char7 >*< char7 >*< char7

{-# INLINE fiveStaticASCIIChars #-}
fiveStaticASCIIChars :: Char -> Char -> Char -> Char -> Char -> FixedPrim a
fiveStaticASCIIChars a b c d e =
  const (a, (b, (c, (d, e)))) >$< char7 >*< char7 >*< char7 >*< char7 >*< char7

{-# INLINE oneStaticASCIIChar #-}
oneStaticASCIIChar :: Char -> FixedPrim a
oneStaticASCIIChar x =
  const x >$< char7
