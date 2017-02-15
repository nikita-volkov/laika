{-|
Parsing of an XML tree with unparsed placeholders into a template.
-}
module Laika.Building.XMLTree where

import Laika.Prelude
import Data.XML.Types
import qualified Laika.Building.HTMLExp as C
import qualified Laika.Building.Template as D
import qualified Data.Text as E
import qualified Data.Attoparsec.Text as A
import qualified HTMLEntities.Decoder as B


{-|
A renderer, which also does parsing.
-}
newtype Interpreter a =
  Interpreter (a -> Either Text C.Builder)

run :: Interpreter a -> a -> Either Text C.Builder
run (Interpreter def) =
  def

document :: Interpreter Document
document =
  Interpreter def
  where
    def (Document prologue root epilogue) =
      run element root

node :: Interpreter Node
node =
  Interpreter def
  where
    def =
      \case
        NodeElement x -> run element x
        NodeContent x -> run (content (Interpreter (Right . C.nodePlaceholder))) x
        NodeComment x -> run comment x
        NodeInstruction x -> run instruction x

element :: Interpreter Element
element =
  Interpreter def
  where
    def (Element elementName attributes nodes) =
      construct <$> run name elementName <*> traverse (run attribute) attributes <*> traverse (run node) nodes
      where
        construct name attributes nodes =
          "<" <> name <> foldMap (\x -> " " <> x) attributes <>
          if null nodes
            then "/>"
            else ">" <> fold nodes <> "</" <> name <> ">"

attribute :: Interpreter (Name, [Content])
attribute =
  Interpreter def
  where
    def (attributeName, attributeContents) =
      construct <$>
      run name attributeName <*>
      traverse (run (content (Interpreter (Right . C.attrPlaceholder)))) attributeContents
      where
        construct name contents =
          if null contents
            then name
            else name <> "=\"" <> fold contents <> "\""

comment :: Interpreter Text
comment =
  Interpreter def
  where
    def text =
      fmap (\x -> "<!--" <> x <> "-->") (Right (C.unescapedText text))

instruction :: Interpreter Instruction
instruction =
  Interpreter def
  where
    def x =
      Left (fromString (showString "Unexpected instruction node: " (show x)))

name :: Interpreter Name
name =
  Interpreter def
  where
    def (Name localName namespace prefix) =
      case prefix of
        Nothing -> Right builder
          where
            builder =
              foldMap (\x -> C.escapedText (E.toCaseFold x) <> ":") namespace <>
              C.escapedText (E.toCaseFold localName)
        Just x -> Left ("Unexpected prefix for a name: " <> x)

content :: Interpreter Text -> Interpreter Content
content placeholder =
  Interpreter def
  where
    def =
      \case
        ContentText x -> Right (C.unescapedText x)
        ContentEntity x -> run (entity placeholder) x

entity :: Interpreter Text -> Interpreter Text
entity placeholder =
  Interpreter def
  where
    def x =
      case E.uncons x of
        Just ('!', x) -> run placeholder x
        _ -> bimap interpretError (const (C.unescapedText ("&" <> x <> ";"))) (B.htmlEntityBody x)
          where
            interpretError error =
              fromString (showString "Entity validation error: &" (showString (E.unpack x) ";"))
