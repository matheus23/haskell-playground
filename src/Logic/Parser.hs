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
import Data.Functor.Foldable

contraposition :: Expr
contraposition = parseExpr "(a -> b) <-> (!b -> !a)"

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
identifier = mkVar <$> lexeme (many letterChar)

expr :: Parser Expr
expr = makeExprParser term operators <?> "expression"

term :: Parser Expr
term = inParens expr
  <|> mkZero <$ symbol "0"
  <|> mkOne <$ symbol "1"
  <|> identifier
  <?> "term"

operators :: [[Operator Parser Expr]]
operators =
  [ [ Prefix (foldr1 (.) <$> some (mkNot <$ pnot)) ]
  , [ InfixR (mkAnd <$ pand) ]
  , [ InfixR (mkOr <$ por) ]
  , [ InfixR (mkEquiv <$ pequiv) ]
  , [ InfixR (mkImply <$ pimply) ]
  ]

pnot, por, pand, pequiv, pimply :: Parser String
pnot   = symbol "!" <|> symbol "¬"
por    = symbol "|" <|> symbol "∨"
pand   = symbol "&" <|> symbol "∧"
pequiv = symbol "<->" <|> symbol "⇔"
pimply = symbol "->" <|> symbol "⇒"
