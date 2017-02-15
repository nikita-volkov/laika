module Laika.Building.ByteString.Builder where

import Laika.Prelude
import Data.ByteString.Builder.Internal
import Foreign
import qualified Data.Text.Encoding as C
import qualified Laika.Building.ByteString.Builder.BoundedPrims as D


{-# INLINE byteListOfLength #-}
byteListOfLength :: Int -> [Word8] -> Builder
byteListOfLength length list =
  ensureFree length <> builder stepUpdate
  where
    stepUpdate :: BuildStep a -> BuildStep a
    stepUpdate nextStep (BufferRange ptr1 ptr2) =
      do
        newPtr1 <- foldlM (\ptr byte -> poke ptr byte $> plusPtr ptr 1) ptr1 list
        nextStep $! BufferRange newPtr1 ptr2

{-# INLINE textInUTF8HTML #-}
textInUTF8HTML :: Text -> Builder
textInUTF8HTML =
  C.encodeUtf8BuilderEscaped D.html
