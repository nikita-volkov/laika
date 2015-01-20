module Laika.Lexer where

import Laika.Prelude
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS


-- * General parsing
-------------------------

type Lexer = Parser

run :: Lexer a -> Text -> Either String a
run p t =
  onResult $ parse p t
  where
    onResult =
      \case
        Fail _ contexts message -> Left $ showString message . showString ". Contexts: " .
                                          shows contexts $ "."
        Done _ a -> Right a
        Partial c -> onResult (c "")

-- |
-- Run a lexer on a given input,
-- lifting its errors to the context lexer.
-- 
-- Consider it a sublexer.
lexer :: Lexer a -> Text -> Lexer a
lexer p t =
  either fail return $
  run p t

complete :: Lexer a -> Lexer a
complete p =
  p <* skipSpace <* endOfInput

labeled :: String -> Lexer a -> Lexer a
labeled =
  flip (<?>)

-- |
-- This lexer does not consume any input.
shouldFail :: Lexer a -> Lexer ()
shouldFail p =
  optional p >>= maybe (return ()) (const empty)

manyFollowedBy :: Lexer a -> Lexer b -> Lexer ([b], a)
manyFollowedBy a b =
  (([],) <$> a) <|> 
  (\br (brl, ar) -> (br : brl, ar)) <$> b <*> manyFollowedBy a b

escapedText :: [Char] -> Lexer Text
escapedText escapedChars =
  T.pack <$> many1 (escapedChar <|> unescapedChar)
  where
    escapedChar =
      char '\\' *> ((satisfy (inClass escapedChars)) <|> char '\\')
    unescapedChar =
      satisfy (notInClass escapedChars)

-- * Path
-------------------------

type Path = 
  [r|{ absolute :: Bool, segments :: [PathSegment] }|]

data PathSegment = 
  Dot | DoubleDot | Identifier Text
  deriving (Show)

path :: Lexer Path
path =
  labeled "path" $ do
    absolute <- True <$ char '/' <|> pure False
    segments <- sepBy1 segment (char '/')
    return $ [r|{ absolute = absolute, segments = segments }|]
  where
    segment =
      DoubleDot <$ string ".." <|>
      Dot <$ char '.' <|>
      Identifier <$> takeWhile1 (/= '/')

-- * FilePath
-------------------------

filePath :: Lexer FilePath
filePath =
  FS.fromText <$> takeText

-- * Reference
-------------------------

type Reference =
  [r|{ escaped :: Bool, path :: Path }|]

reference :: Lexer Reference
reference =
  labeled "reference" $ do
    char '{'
    skipSpace
    e <- False <$ (asciiCI "unescaped" <* skipMany1 space) <|> 
         pure True
    p <- lexer (complete path) =<< 
         ((char '"' *> escapedText ['"'] <* char '"') <|>
          (escapedText ['}', ':']))
    skipSpace
    char '}'
    return $ [r|{ escaped = e, path = p }|]

-- * Include
-------------------------

include :: Lexer FilePath
include =
  labeled "include" $ do
    char '{'
    skipSpace
    asciiCI "include"
    skipMany1 space
    p <- lexer filePath =<< 
         ((char '"' *> escapedText ['"'] <* char '"') <|>
          (escapedText ['}']))
    skipSpace
    char '}'
    return $ p

-- * Block
-------------------------

blockOpening :: Lexer Path
blockOpening =
  labeled "blockOpening" $ do
    char '{'
    skipSpace
    p <- lexer (complete path) =<< 
         ((char '"' *> escapedText ['"'] <* char '"') <|>
          (escapedText [':']))
    skipSpace
    char ':'
    skipSpace
    char '}'
    return p

-- * Template
-------------------------

data Token =
  Text Text |
  Reference Reference |
  BlockOpening Path |
  BlockClosing |
  Include FilePath 
  deriving (Show)

tokens :: Lexer [Token]
tokens = 
  many chunk
  where
    chunk =
      Text <$> escapedText ['{'] <|>
      Reference <$> reference <|>
      BlockOpening <$> blockOpening <|>
      BlockClosing <$ (char '{' *> skipSpace *> char ':' *> skipSpace *> char '}') <|>
      Include <$> include

