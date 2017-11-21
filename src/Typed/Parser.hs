{-# LANGUAGE RankNTypes #-}
module Typed.Parser where

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Maybe (fromJust, maybe)
import Data.String
import Control.Monad (void)
import Control.Applicative hiding (Const)
import Data.Functor.Foldable
import qualified Data.Map as Map
import Data.Map (Map, (!?))

import Typed.Lambda

parseExpr :: String -> Expr
parseExpr str =
  case runParser expr "<interactive>" (removeAnsiTerminalCodes str) of
    Right expr -> expr
    Left err -> error $ "Parsing error: " ++ show err

type Parser = Parsec Void String
type IndexEnv = Map Name Int

data ParsedExpr
  = PType
  | PVar Name
  | PLambda Name ParsedExpr ParsedExpr
  | PPi Name ParsedExpr ParsedExpr
  | PApp ParsedExpr ParsedExpr
  | PArrow ParsedExpr ParsedExpr
  deriving (Show, Eq, Read)

computeExpr :: IndexEnv -> ParsedExpr -> Expr
computeExpr env PType = Fix (Const Type)
computeExpr env (PVar name) = Fix (maybe (FreeVar name) (flip BoundVar name) (env !? name))
computeExpr env (PLambda name typ body) =
  let t = computeExpr env typ
  in Fix (Lambda name t (computeExpr (advanceEnv name env) body))
computeExpr env (PPi name typ body) =
  let t = computeExpr env typ
  in Fix (Pi name t (computeExpr (advanceEnv name env) body))
computeExpr env (PApp func arg) =
  Fix (computeExpr env func :@ computeExpr env arg)
computeExpr env (PArrow arg res) =
  let t = computeExpr env arg
  in Fix (Pi "_" t (computeExpr (advanceEnv "_" env) res))

advanceEnv :: Name -> IndexEnv -> IndexEnv
advanceEnv introduced = Map.insert introduced 0 . fmap (+ 1)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

inParens :: Parser a -> Parser a
inParens = between (symbol "(") (symbol ")")

name :: Parser Name
name = lexeme (some (oneOf identifierChars)) -- TODO: Disallow "->" as name

identifierChars :: String
identifierChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'+-*/$%&|><=?!#"

expr :: Parser Expr
expr = computeExpr Map.empty <$> parsedExpr

parsedExpr :: Parser ParsedExpr
parsedExpr =
  makeExprParser nonInfixExpr operators
  <|> nonInfixExpr

nonInfixExpr :: Parser ParsedExpr
nonInfixExpr =
  constants
  <|> makeFixApp <$> application nonApplication
  <|> nonApplication
  where
    makeFixApp (x, xs) = foldl PApp x xs

constants :: Parser ParsedExpr
constants =
  PType <$ symbol "Type"

nonApplication :: Parser ParsedExpr
nonApplication =
  abstraction (symbol "\\" <|> symbol "λ") PLambda
  <|> abstraction (symbol "|~|" <|> symbol "Π") PPi
  <|> forallType
  <|> inParens parsedExpr
  <|> PVar <$> name

abstraction :: Parser String -> (Name -> ParsedExpr -> ParsedExpr -> ParsedExpr) -> Parser ParsedExpr
abstraction abstractionSymbol abstract = do
  abstractionSymbol
  arg <- name
  symbol ":"
  t <- parsedExpr
  symbol "."
  body <- parsedExpr
  return (abstract arg t body)

application :: Parser a -> Parser (a, [a])
application parseNonApp = do
  func <- parseNonApp
  args <- many parseNonApp
  return (func, args)

forallType :: Parser ParsedExpr
forallType = do
  symbol "forall" <|> symbol "∀"
  n <- name
  symbol "."
  t <- parsedExpr
  return (PPi n PType t)

operators :: [[Operator Parser ParsedExpr]]
operators =
  [ [ InfixR (PArrow <$ arrow) ] ]

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
