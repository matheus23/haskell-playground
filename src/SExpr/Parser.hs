module SExpr.Parser where

import Control.Monad (void)
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.String
import Data.Maybe

import Data.Functor.Foldable

import SExpr.Types

parseSExpr :: String -> SExpr
parseSExpr = fromJust . parseMaybe sExprParser

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

inParens :: Parser a -> Parser a
inParens = between (symbol "(") (symbol ")")

identifierChars :: String
identifierChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'+-*/$%&|><=?!#"

sExprSymbol :: Parser (SExprF a)
sExprSymbol = Symbol <$> lexeme (some (oneOf identifierChars))

sExprList :: Parser a -> Parser (SExprF a)
sExprList elementParser =
  List <$> inParens (many elementParser)

sExprParserAlg :: Parser a -> Parser (SExprF a)
sExprParserAlg elementParser = sExprSymbol <|> sExprList elementParser

sExprParser :: Parser SExpr
sExprParser = fix (fmap Fix . sExprParserAlg)
  where fix f = f (fix f)
