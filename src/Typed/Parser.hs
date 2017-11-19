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
import qualified Data.Map as Map
import Data.Map (Map, (!?))

import Typed.Lambda

parseExpr :: String -> Expr
parseExpr str =
  case runParser (expr Map.empty) "<interactive>" (removeAnsiTerminalCodes str) of
    Right expr -> expr
    Left err -> error $ "Parsing error: " ++ show err

type Parser = Parsec Void String
type Env = Map Name Int

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

expr :: Env -> Parser Expr
expr env =
  constants
  <|> makeFixApp <$> application (nonApplication env)
  <|> nonApplication env
  -- <|> makeExprParser (term typeEnv) operators
  where
    makeFixApp (x, xs) = foldl (\a b -> Fix (a :@ b)) x xs

constants :: Parser Expr
constants =
  Fix Type <$ symbol "Type"

nonApplication :: Env -> Parser Expr
nonApplication env =
  abstraction env (symbol "\\" <|> symbol "λ") Lambda
  <|> abstraction env (symbol "|~|" <|> symbol "Π") Pi
  <|> forallType env
  <|> inParens (expr env)
  <|> constructVar <$> name
  where
    constructVar name =
      case env !? name of
        Just index -> Fix (BoundVar index name)
        Nothing -> Fix (FreeVar name)

abstraction :: Env -> Parser String -> (Name -> ExprF Expr -> Expr -> ExprF Expr) -> Parser Expr
abstraction env abstractionSymbol abstract = do
  abstractionSymbol
  arg <- name
  symbol ":"
  (Fix t) <- expr env
  symbol "."
  body <- expr (advanceEnv arg env)
  return (Fix (abstract arg t body))

advanceEnv :: Name -> Map Name Int -> Map Name Int
advanceEnv introduced = Map.insert introduced 0 . fmap (+ 1)

application :: Parser a -> Parser (a, [a])
application parseNonApp = do
  func <- parseNonApp
  args <- many parseNonApp
  return (func, args)

forallType :: Map Name Int -> Parser Expr
forallType env = do
  symbol "\\/" <|> symbol "∀"
  n <- name
  symbol "."
  t <- expr (advanceEnv n env)
  return (Fix (Pi n Type t))

{-
typ :: Map Name Int -> Parser Type
typ typeEnv =
  forallType typeEnv
  <|> makeExprParser (term typeEnv) operators <?> "type"

term :: Map Name Int -> Parser Expr
term typeEnv =
  inParens (typ typeEnv)
  <|> constructTypeVar <$> name <?> "type variable"
  where
    constructTypeVar name =
      case typeEnv !? name of
        Just index -> Fix (TypeVar index name)
        Nothing -> Fix (FreeType name)

operators :: [[Operator Parser Expr]]
operators =
  [ [ InfixR (mkArrow <$ arrow) ] ]
  where mkArrow l r = Fix (Pi "_" l r)
-}

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
