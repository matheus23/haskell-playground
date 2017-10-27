{-# LANGUAGE OverloadedStrings #-}
module Logic.Parser where

import Logic.Propositional

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Applicative
import Data.String
import Data.Maybe

contraposition :: Expr
contraposition = "(a -> b) <-> (!b -> !a)"

instance IsString Expr where
  fromString = parseExpr

parseExpr :: String -> Expr
parseExpr = fromJust . parseMaybe expr

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

inParens :: Parser a -> Parser a
inParens = between (symbol "(") (symbol ")")

identifier :: Parser Expr
identifier = Var <$> lexeme (many letterChar)

expr :: Parser Expr
expr = makeExprParser term operators <?> "expression"

term :: Parser Expr
term = inParens expr
  <|> Zero <$ symbol "0"
  <|> One <$ symbol "1"
  <|> identifier
  <?> "term"

operators :: [[Operator Parser Expr]]
operators =
  [ [ Prefix (foldr1 (.) <$> some (Not <$ pnot)) ]
  , [ InfixR (Op And <$ pand) ]
  , [ InfixR (Op Or <$ por) ]
  , [ InfixR (Op Equiv <$ pequiv) ]
  , [ InfixR (Op Imply <$ pimply) ]
  ]

pnot, por, pand, pequiv, pimply :: Parser String
pnot   = symbol "!" <|> symbol "¬"
por    = symbol "|" <|> symbol "∨"
pand   = symbol "&" <|> symbol "∧"
pequiv = symbol "<->" <|> symbol "⇔"
pimply = symbol "->" <|> symbol "⇒"
