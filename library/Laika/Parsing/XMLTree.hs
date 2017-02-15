module Laika.Parsing.XMLTree where

import Laika.Prelude
import Data.XML.Types
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Text.Lazy as D
import qualified Attoparsec.XML as B
import qualified Data.Text.Lazy as C


text :: Text -> Either Text [Node]
text =
  first interpretError . A.parseOnly (B.nodes <* A.endOfInput)
  where
    interpretError x =
      fromString (showString "XML parsing error: " x)

lazyText :: C.Text -> Either Text [Node]
lazyText =
  first interpretError . D.eitherResult . D.parse (B.nodes <* A.endOfInput)
  where
    interpretError x =
      fromString (showString "XML parsing error: " x)
