module Laika.Parser where

import Laika.Prelude hiding (lex)
import qualified Laika.Lexer as Lexer
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS


type Parser =
  StateT [Lexer.Token] (EitherT String IO)

run :: Parser a -> IO (Either String a)
run =
  runEitherT .
  flip evalStateT []

lex :: Text -> Parser ()
lex =
  join . fmap (\l -> modify (l <>)) .
  lift . hoistEither . Lexer.run (Lexer.complete Lexer.tokens)

load :: FilePath -> Parser ()
load p =
  liftIO (FS.isFile p) >>= \case
    False -> lift $ left $ showString "File not found: " $ show p
    True -> liftIO (FS.readTextFile p) >>= lex

parseTemplate :: Parser Template
parseTemplate =
  do phrase <- Just <$> parsePhrase <|> pure Nothing
     maybe (return []) (\x -> (x :) <$> parseTemplate) phrase

parsePhrase :: Parser Phrase
parsePhrase =
  getToken >>= \case
    Lexer.BlockOpening path ->
      do template <- parseTemplate
         getToken >>= \case 
           Lexer.BlockClosing -> return ()
           _ -> lift $ left $ "Unclosed block"
         return $ 
           let path' = convertPath path
               in Block [r|{ path = path', template = template }|]
    Lexer.BlockClosing ->
      lift $ left $ "Unexpected block closing"
    Lexer.Include path ->
      do load path
         parsePhrase
    Lexer.Reference ref ->
      return $ Reference $ convertReference ref
    Lexer.Text t ->
      return $ Text t

getToken :: Parser Lexer.Token
getToken =
  StateT $ \case
    h : t -> return (h, t)
    _ -> left $ "No tokens left"

shouldFail :: Parser a -> Parser ()
shouldFail p =
  optional p >>= maybe (return ()) (const empty)

endOfInput :: Parser ()
endOfInput =
  optional getToken >>= 
  maybe (return ()) 
        (\t -> lift $ left $ "Not all tokens got parsed. Stopped at: " <> show t)

convertPath :: Lexer.Path -> Path
convertPath lp =
  let absolute = view [l|absolute|] lp
      segments = map convertPathSegment $ view [l|segments|] lp
      in [r|{ absolute = absolute, segments = segments }|]
  where
    convertPathSegment =
      \case
        Lexer.Dot -> Dot
        Lexer.DoubleDot -> DoubleDot
        Lexer.Identifier t -> Identifier t

convertReference :: Lexer.Reference -> Reference
convertReference lr =
  let escaped = view [l|escaped|] lr
      path = convertPath $ view [l|path|] lr
      in [r|{ escaped = escaped, path = path }|]


-- * Model
-------------------------

type Template =
  [Phrase]

data Phrase =
  Text Text | 
  Reference Reference |
  Block Block 

deriving instance Show Phrase

type Reference =
  [r|{ escaped :: Bool, path :: Path }|]

type Path = 
  [r|{ absolute :: Bool, segments :: [PathSegment] }|]

data PathSegment = 
  Dot | DoubleDot | Identifier Text
  deriving (Show)

type Block =
  [r|{ path :: Path, template :: Template }|]

