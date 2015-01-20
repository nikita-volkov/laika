module Main where

import BasePrelude
import Record
import qualified Laika as Laika
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLI
import qualified Data.Text.Lazy.Builder as TLB


main =
  TLI.putStrLn $ TLB.toLazyText $ render $ model

model =
  [r| {
        name = { primary = "Laika", original = "Kudryavka" },
        followers = Just [{ name = "Belka" }, { name = "Strelka" }]
      } |]

render =
  $(Laika.file "demo/template.html.laika")

