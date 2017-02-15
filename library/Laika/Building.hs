{-|
A DSL for building HTML.
-}
module Laika.Building
(
  A.HTML,
  A.text,
  html,
  htmlFile,
)
where

import Laika.Prelude
import qualified Laika.Building.Template as A
import qualified Data.ByteString.Builder as B
import qualified Language.Haskell.TH.Quote as C
import qualified Laika.Building.XMLTree as D
import qualified Laika.Parsing.XMLTree as E
import qualified Laika.Building.HTMLExp as F
import qualified Data.Text as G


{-|
A quasi-quoter, which produces an expression of type 'A.HTML'.
It parses and validates HTML and reconstructs it into well-formed HTML
with all the entities encoded, tags properly closed and etc.
All the entities of form @&!name;@ get interpreted as placeholders for variables
to interpolate.

The reconstruction is done at compile-time,
which gives you the following benefits:

* Your HTML is __checked at compile-time__,
ensuring that if your code compiles, it produces valid HTML,

* All the encoding and HTML-generation is done at compile-time as well,
squeezing the absolute __maximum of runtime performance__ for your application.

Depending on the context, the placeholder can have the following types:

* In the context of an element content, it has the type 'A.HTML',

* In the context of an attribute it has the type 'Text'.

[Example]

>myHTML :: Text -> HTML -> HTML
>myHTML var1 var2 =
>  [html|<a href="&!var1;">&!var2;</a>|]
-}
html :: C.QuasiQuoter
html =
  C.QuasiQuoter exp undefined undefined undefined
  where
    exp x =
      either (fail . G.unpack) return expEither
      where
        expEither =
          E.text (fromString x) >>= traverse (D.run D.node) >>= return . F.run . fold

{-|
Does all the same as 'html' with a difference
that in the quote you specify the path to a file,
which contains the template.

[Example]

>myHTML :: Text -> HTML -> HTML
>myHTML var1 var2 =
>  [htmlFile|src/html.html|]
-}
htmlFile :: C.QuasiQuoter
htmlFile =
  C.quoteFile html
