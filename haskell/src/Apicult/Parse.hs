{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Apicult.Parse
  ( parseApi,
    Api (..),
    Variable (..),
    variablesWithDefaults,
    Endpoint (..),
    Result (..),
    Request (..),
    Method (..),
    Body (..),
    BodyType (..),
    Pair (..),
    Interpolated (..),
    InterpolatedPart (..),
    apiParser,
  )
where

import Control.Exception (throwIO)
import Data.Char (isAlphaNum)
import Data.Text qualified as T
import GHC.Exception (errorCallWithCallStackException)
import Language.Haskell.TH.Syntax (Lift)
import Relude.Extra (groupBy, lookup)
import Text.Megaparsec
  ( MonadParsec (eof, takeWhile1P, try),
    Parsec,
    anySingle,
    anySingleBut,
    between,
    errorBundlePretty,
    many,
    manyTill,
    match,
    parse,
    some,
  )
import Text.Megaparsec.Char (alphaNumChar, char, newline, printChar, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (many, some)

type Parser = Parsec Void Text

data Api = Api
  { globals :: [Variable],
    endpoints :: [Endpoint]
  }
  deriving stock (Show, Lift)

data Variable = Variable {name :: Text, defaultValue :: Maybe Text} deriving stock (Show, Lift)

variablesWithDefaults :: [Variable] -> Map Text Text
variablesWithDefaults =
  fromList
    . mapMaybe
      ( \Variable {name, defaultValue} ->
          do
            defV <- defaultValue
            return (name, defV)
      )

data Endpoint = Endpoint
  { name :: Text,
    doc :: Text,
    variables :: [Variable],
    request :: Request,
    result :: Result
  }
  deriving stock (Show, Lift)

data Result
  = JsonResult Void
  | Expectation [Text]
  | NoResult
  deriving stock (Show, Lift)

data Request = Request
  { method :: Method,
    url :: Interpolated,
    querystring :: [Pair],
    headers :: [Pair],
    body :: Body
  }
  deriving stock (Show, Lift)

data Method
  = Get
  | Post
  deriving stock (Show, Lift)

data Body = Body
  { bodyType :: BodyType,
    content :: [Pair]
  }
  deriving stock (Show, Lift)

data BodyType
  = Form
  | Json
  deriving stock (Show, Lift)

data Pair = Pair {key :: Text, value :: Interpolated} deriving stock (Show, Lift)

newtype Interpolated = Interpolated [InterpolatedPart] deriving stock (Show, Lift)

data InterpolatedPart
  = Literal Text
  | Var Text
  deriving stock (Show, Lift)

parseApi :: (HasCallStack, MonadIO m) => FilePath -> m Api
parseApi f = do
  content <- liftIO $ decodeUtf8 <$> readFileBS f
  case parse apiParser f content of
    Right r -> return r
    Left e -> liftIO $ throwIO $ errorCallWithCallStackException (errorBundlePretty e) ?callStack

apiParser :: Parser Api
apiParser = do
  spaceConsumer
  void $ many newline
  globals <- many variable
  endpoints <- many endpoint
  eof
  return Api {..}

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    (void $ takeWhile1P (Just "white space") (== ' '))
    (L.skipLineComment "##" *> void newlines)
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

variable :: Parser Variable
variable =
  lexeme $
    do
      name <- T.pack <$> some (alphaNumChar <|> char '_')
      defaultValue <- Just <$> (char '=' *> (T.pack <$> manyTill printChar newlines)) <|> Nothing <$ newlines
      return Variable {..}

endpoint :: Parser Endpoint
endpoint =
  lexeme $ do
    (doc, (name, variables, request, result)) <- match $ do
      void $ lexeme $ string "#"
      name <- T.pack <$> manyTill printChar newlines
      variables <- many variable
      request <- requestP
      result <- resultP
      return (name, variables, request, result)
    return Endpoint {..}

requestP :: Parser Request
requestP =
  lexeme $ do
    void $ lexeme $ string ">"
    void $ optional $ lexeme $ string "http"
    formBody <- optional $ lexeme (Form <$ "-f")
    explicitMethod <- optional $ lexeme methodP
    url <- interpolated
    let optionP =
          lexeme $ do
            key <- T.pack <$> some (alphaNumChar <|> char '_')
            symbol <- ":" <|> try "==" <|> "="
            value <- interpolated
            return (symbol, Pair key value)
    options <- fmap (fmap snd) . groupBy fst <$> many optionP :: Parser (Map Text (NonEmpty Pair))
    let opt symbol = maybe [] toList $ lookup symbol options
        querystring = opt "=="
        headers = opt ":"
        bodyParts = opt "="
    newlines
    let method = fromMaybe (if null bodyParts then Get else Post) explicitMethod
        bodyType = fromMaybe Json formBody
        body = Body bodyType bodyParts
    return Request {..}

methodP :: Parser Method
methodP = lexeme (Get <$ string "GET" <|> Post <$ string "POST")

resultP :: Parser Result
resultP = expectation <|> jsonResult <|> return NoResult
  where
    expectation =
      Expectation <$> do
        void $ string "expect"
        newlines
        some lineThatIsntANewEndpoint
    lineThatIsntANewEndpoint = do
      firstChar <- anySingleBut '#'
      rest <- manyTill printChar newlines
      return (T.pack (firstChar : rest))
    jsonResult = NoResult <$ (char '{' *> manyTill anySingle "\n}" *> many newline)

interpolated :: Parser Interpolated
interpolated =
  lexeme $
    Interpolated
      <$> ( between "\"" "\"" partsWithQuotes
              <|> partsWithoutQuotes
          )
  where
    partsWithQuotes, partsWithoutQuotes :: Parser [InterpolatedPart]
    partsWithQuotes = some (var <|> Literal <$> takeWhile1P (Just "non-(quote,dollar,newline))") (\c -> c /= '"' && c /= '$' && c /= '\n'))
    partsWithoutQuotes = some (var <|> Literal <$> takeWhile1P (Just "non-(space-dollar,newline)") (\c -> c /= ' ' && c /= '$' && c /= '\n'))
    var :: Parser InterpolatedPart
    var = Var <$> (char '$' *> takeWhile1P (Just "alphanum or underscore") (\c -> isAlphaNum c || c == '_'))

newlines :: Parser ()
newlines = lexeme $ void $ some newline