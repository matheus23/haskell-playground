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
  case runParser (lambda (Map.empty, Map.empty)) "<interactive>" (removeAnsiTerminalCodes str) of
    Right expr -> expr
    Left err -> error $ "Parsing error: " ++ show err

parseType :: String -> Type
parseType str =
  case runParser (typ Map.empty) "<interactive>" (removeAnsiTerminalCodes str) of
    Right typ -> typ
    Left err -> error $ "Parsing error: " ++ show err

type Parser = Parsec Void String
type Env = (Map Name Int, Map Name Int)

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

lambda :: Env -> Parser Expr
lambda envs =
  makeFixApp <$> application (nonApplication envs)
  <|> nonApplication envs
  where
    makeFixApp (x, xs) = foldl (\a b -> Fix (a :@ b)) x xs

nonApplication :: Env -> Parser Expr
nonApplication (typeEnv, valueEnv) =
  abstraction (typeEnv, valueEnv)
  <|> forallAbs (typeEnv, valueEnv)
  <|> inParens (lambda (typeEnv, valueEnv))
  <|> constructVar <$> name
  where
    constructVar name =
      case valueEnv !? name of
        Just index -> Fix (Var index name)
        Nothing -> Fix (FreeVar name)

abstraction :: Env -> Parser Expr
abstraction (typeEnv, valueEnv) = do
  symbol "\\" <|> symbol "λ"
  arg <- name
  symbol ":"
  t <- typ typeEnv
  symbol "."
  body <- lambda (typeEnv, advanceEnv arg valueEnv)
  return (Fix (Abs arg t body))

forallSymbol :: Parser ()
forallSymbol = symbol "/\\" <|> symbol "Λ" >> return ()

forallAbs :: Env -> Parser Expr
forallAbs (typeEnv, valueEnv) = do
  forallSymbol
  n <- name
  symbol "."
  body <- lambda (advanceEnv n typeEnv, valueEnv)
  return (Fix (ForallAbs n body))

advanceEnv :: Name -> Map Name Int -> Map Name Int
advanceEnv introduced = Map.insert introduced 0 . fmap (+ 1)

application :: Parser a -> Parser (a, [a])
application parseNonApp = do
  func <- parseNonApp
  args <- many parseNonApp
  return (func, args)

typ :: Map Name Int -> Parser Type
typ typeEnv =
  forallType typeEnv
  <|> makeExprParser (term typeEnv) operators <?> "type"

forallType :: Map Name Int -> Parser Type
forallType typeEnv = do
  symbol "/\\" <|> symbol "Λ"
  n <- name
  symbol "."
  t <- typ (advanceEnv n typeEnv)
  return (Fix (ForallType n t))

term :: Map Name Int -> Parser Type
term typeEnv =
  inParens (typ typeEnv)
  <|> constructTypeVar <$> name <?> "type variable"
  where
    constructTypeVar name =
      case typeEnv !? name of
        Just index -> Fix (TypeVar index name)
        Nothing -> Fix (FreeType name)

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
