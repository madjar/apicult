{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Apicult.Parse
  ( Api (..),
    Variable (..),
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

import Data.Char (isSpace)
import qualified Data.Text as T
import Relude.Extra (groupBy, lookup)
import Text.Megaparsec
  ( MonadParsec (eof, takeWhile1P, try),
    Parsec,
    anySingleBut,
    between,
    many,
    manyTill,
    some,
  )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, newline, printChar, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (many, some)

type Parser = Parsec Void Text

data Api = Api
  { globals :: [Variable],
    endpoints :: [Endpoint]
  }
  deriving stock (Show)

data Variable = Variable {name :: Text, defaultValue :: Text} deriving stock (Show)

data Endpoint = Endpoint
  { name :: Text,
    variables :: [Variable],
    request :: Request,
    result :: Result
  }
  deriving stock (Show)

data Result
  = JsonResult Void
  | Expectation [Text]
  | NoResult
  deriving stock (Show)

data Request = Request
  { method :: Method,
    url :: Interpolated,
    querystring :: [Pair],
    headers :: [Pair],
    body :: Body
  }
  deriving stock (Show)

data Method
  = Get
  | Post
  deriving stock (Show)

data Body = Body
  { bodyType :: BodyType,
    content :: [Pair]
  }
  deriving stock (Show)

data BodyType
  = Form
  | Json
  deriving stock (Show)

data Pair = Pair {key :: Text, value :: Interpolated} deriving stock (Show)

newtype Interpolated = Interpolated [InterpolatedPart] deriving stock (Show)

data InterpolatedPart
  = Literal Text
  | Var Text
  deriving stock (Show)

apiParser :: Parser Api
apiParser = do
  spaceConsumer
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
      name <- T.pack <$> some letterChar
      void $ char '='
      defaultValue <- T.pack <$> manyTill printChar newlines
      return Variable {..}

endpoint :: Parser Endpoint
endpoint =
  lexeme $
    do
      void $ lexeme $ string "#"
      name <- T.pack <$> manyTill printChar newlines
      variables <- many variable
      request <- requestP
      result <- resultP
      return Endpoint {..}

requestP :: Parser Request
requestP =
  lexeme $ do
    void $ lexeme $ string ">"
    void $ optional $ lexeme $ string "http"
    formBody <- optional $ lexeme (Form <$ "-f")
    explicitMethod <- optional methodP
    url <- badMakeInterpolated <$> lexeme (takeWhile1P Nothing (not . isSpace))
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
resultP = expectation <|> return NoResult
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

-- TODO handle interpolated
interpolated :: Parser Interpolated
interpolated =
  badMakeInterpolated
    <$> ( between "\"" "\"" (takeWhile1P (Just "non-quote character") (/= '"'))
            <|> takeWhile1P (Just "non-space character") (not . isSpace)
        )

badMakeInterpolated :: Text -> Interpolated
badMakeInterpolated = Interpolated . pure . Literal

newlines :: Parser ()
newlines = void $ some newline