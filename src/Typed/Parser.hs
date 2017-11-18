module Typed.Parser where

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Maybe (fromJust)
import Data.String
import Control.Monad (void)
import Control.Applicative
import Data.Functor.Foldable

import Typed.Lambda

parseExpr :: String -> Expr
parseExpr str =
  case runParser lambda "<interactive>" (removeAnsiTerminalCodes str) of
    Right expr -> expr
    Left err -> error $ "Parsing error: " ++ show err

parseType :: String -> Type
parseType str =
  case runParser typ "<interactive>" (removeAnsiTerminalCodes str) of
    Right typ -> typ
    Left err -> error $ "Parsing error: " ++ show err

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

lambda :: Parser Expr
lambda =
  makeFixApp <$> application (nonApplication lambda)
  <|> nonApplication lambda
  where
    makeFixApp (x, xs) = foldl (\a b -> Fix (a :@ b)) x xs

nonApplication :: Parser Expr -> Parser Expr
nonApplication parseInner =
  Fix <$> abstraction parseInner
  <|> inParens parseInner
  <|> Fix . Var <$> name

abstraction :: Parser a -> Parser (ExprF a)
abstraction parseInner = do
  symbol "\\" <|> symbol "λ"
  arg <- name
  symbol ":"
  t <- typ
  symbol "."
  body <- parseInner
  return (Abs arg t body)

application :: Parser a -> Parser (a, [a])
application parseNonApp = do
  func <- parseNonApp
  args <- many parseNonApp
  return (func, args)

typ :: Parser Type
typ = makeExprParser term operators <?> "type"

term :: Parser Type
term = inParens typ
  <|> (Fix . TypeVar <$> name) <?> "type variable"

operators :: [[Operator Parser Type]]
operators =
  [ [ InfixR (mkArrow <$ arrow) ] ]
  where mkArrow l r = Fix (l :-> r)

arrow :: Parser String
arrow = symbol "->" <|> symbol "→"

removeAnsiTerminalCodes :: String -> String
removeAnsiTerminalCodes xs = removeInEscape xs False

removeInEscape :: String -> Bool -> String
removeInEscape [] _ = []
removeInEscape ('m'   :xs) True  = removeInEscape xs False
removeInEscape (x     :xs) True  = removeInEscape xs True
removeInEscape ('\x1b':xs) False = removeInEscape xs True
removeInEscape (x     :xs) False = x:removeInEscape xs False
