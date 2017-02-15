module Laika.Prelude
( 
  module Exports,
  list,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (First(..), Last(..), (<>))

-- semigroups
-------------------------
import Data.Semigroup as Exports

-- transformers
-------------------------
import Control.Monad.IO.Class as Exports
import Control.Monad.Trans.Class as Exports
import Control.Monad.Trans.Maybe as Exports hiding (liftListen, liftPass)
import Control.Monad.Trans.Reader as Exports hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.State.Strict as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Data.Functor.Identity as Exports

-- mtl
-------------------------
import Control.Monad.Error.Class as Exports (MonadError (..))

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- text
-------------------------
import Data.Text as Exports (Text)


{-# INLINE list #-}
list :: a -> (b -> [b] -> a) -> [b] -> a
list nil cons =
  \case
    head : tail -> cons head tail
    _ -> nil
