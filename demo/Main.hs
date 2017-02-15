module Main where

import Prelude
import Laika.Building
import qualified Laika.Rendering.ByteString.Lazy as A
import qualified Data.ByteString.Lazy.Char8 as C


main =
  C.putStrLn (A.html doc)

doc :: HTML
doc =
  html3 (html2 "&nbsp;" html1)

html1 :: HTML
html1 =
  [html|<p>Аз есмь <b>царь</b>!</p>|]

html2 :: Text -> HTML -> HTML
html2 var1 var2 =
  [html|<a href="&!var1;">&!var2;&nbsp;</a>|]

html3 :: HTML -> HTML
html3 body =
  [htmlFile|demo/template3.html|]
