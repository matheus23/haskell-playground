{-# LANGUAGE TypeOperators #-}
module Lambda.Parser where

import Lambda.Calculus

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Maybe (fromJust)
import Control.Monad (void)
import Control.Applicative

parseLambda :: String -> Lambda
parseLambda = fromJust . parseMaybe lambda

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

inParens :: Parser a -> Parser a
inParens = between (symbol "(") (symbol ")")

name :: Parser Name
name = lexeme (some (alphaNumChar <|> char '_'))

lambda :: Parser Lambda
lambda =
  (application <?> "application")
  <|> nonApplication

nonApplication :: Parser Lambda
nonApplication =
  inParens lambda
  <|> (abstraction <?> "abstraction")
  <|> (Var <$> name <?> "variable")

abstraction :: Parser Lambda
abstraction = do
  symbol "\\" <|> symbol "λ"
  arg <- name
  symbol "."
  body <- lambda
  return (Abs arg body)

application :: Parser Lambda
application = do
  func <- nonApplication
  args <- many nonApplication
  return (foldl App func args)
