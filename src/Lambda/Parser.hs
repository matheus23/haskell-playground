module Lambda.Parser where

import Lambda.Untyped

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Maybe (fromJust)
import Data.String
import Control.Monad (void)
import Control.Applicative

instance IsString Lambda where
  fromString = parseLambda

parseLambda :: String -> Lambda
parseLambda = fromJust . parseMaybe lambda . removeAnsiTerminalCodes

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
name = lexeme (some (oneOf identifierChars))

identifierChars :: String
identifierChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'+-*/$%&|><=?!#"

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

removeAnsiTerminalCodes :: String -> String
removeAnsiTerminalCodes xs = removeInEscape xs False

removeInEscape :: String -> Bool -> String
removeInEscape [] _ = []
removeInEscape ('m'   :xs) True  = removeInEscape xs False
removeInEscape (x     :xs) True  = removeInEscape xs True
removeInEscape ('\x1b':xs) False = removeInEscape xs True
removeInEscape (x     :xs) False = x:removeInEscape xs False
