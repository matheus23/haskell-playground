module Typed.Interpreter where

import Typed.Lambda
import Typed.TypeCheck
import Typed.PrettyPrint
import Typed.Parser

import qualified Data.Map as Map
import Data.Map (Map, (!?))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Functor.Foldable
import Control.Monad

data Value
  = Prim Primitive
  | Fun (Value -> Value)

instance Show Value where
  show (Prim p) = show p
  show (Fun _) = "<function>"

data Primitive
  = Integer Integer
  | Bool Bool
  deriving Show

interpretString :: String -> IO ()
interpretString str =
  let parsed = parseExpr str
      indexed = computeIndexed parsed
   in case typeCheck stdTypes indexed of
        Right typ -> do
          putStrLn $ "Has type: " ++ render 40 (prettyPrintType typ)
          print $ interpret stdValues indexed
        Left error ->
          putStrLn $ prettyErr error

interpret :: (Name -> Value) -> IndexedExpr -> Value
interpret interpretFree expr = fold (interpretAlg interpretFree) expr Map.empty

interpretAlg :: (Name -> Value) -> IndexedExprF (Map Int Value -> Value) -> Map Int Value -> Value
interpretAlg _ (BoundVar index name) env =
  fromMaybe
    (error $ "incorrectly typed (free variable " ++ show name ++ "had no value)")
    (env !? index)
interpretAlg _ (Expr (interpretFunc :@ interpretArg)) env =
  case interpretFunc env of
    Fun run -> run (interpretArg env)
    _ -> error "incorrectly typed (called something that was not a function)"
interpretAlg _ (Expr (Abs name typ interpretBody)) env =
  Fun $ \arg -> interpretBody (Map.insert 0 arg (Map.mapKeys (+ 1) env))
interpretAlg interpretFree (Expr (Var name)) env = interpretFree name

std :: Name -> (TypeCheck Type, Value)
std "+" = primitive "Int -> Int -> Int" $ Prim . Integer <$> ((+) <$> integer <*> integer)
std "-" = primitive "Int -> Int -> Int" $ Prim . Integer <$> ((-) <$> integer <*> integer)
std "*" = primitive "Int -> Int -> Int" $ Prim . Integer <$> ((*) <$> integer <*> integer)
std "//" = primitive "Int -> Int -> Int" $ Prim . Integer <$> (div <$> integer <*> integer)
std "negate" = primitive "Int -> Int" $ Prim . Integer <$> (negate <$> integer)
std "&&" = primitive "Bool -> Bool -> Bool" $ Prim . Bool <$> ((&&) <$> bool <*> bool)
std "==" = primitive "Int -> Int -> Bool" $ Prim . Bool <$> ((==) <$> integer <*> integer)
std "if" = primitive "Bool -> Int -> Int -> Int" $ Prim . Integer <$> (runIf <$> bool <*> integer <*> integer)
std literal =
  case listToMaybe (map fst ((reads :: ReadS Integer) literal)) of
    Just num -> primitive "Int" (pure $ Prim $ Integer num)
    Nothing -> error $ "Unkown literal: " ++ show literal

runIf :: Bool -> Integer -> Integer -> Integer
runIf True l _ = l
runIf False _ r = r

stdTypes :: Name -> TypeCheck Type
stdTypes = fst . std

stdValues :: Name -> Value
stdValues = snd . std

primitive :: String -> FromValue Value -> (TypeCheck Type, Value)
primitive typeName val = (return (parseType typeName), makeValue val)

data FromValue a
  = Pure a
  | ValueFunc (Value -> FromValue a)
  deriving Functor

makeValue :: FromValue Value -> Value
makeValue (Pure val) = val
makeValue (ValueFunc f) = Fun (makeValue . f)

instance Monad FromValue where
  (Pure v) >>= f = f v
  (ValueFunc func) >>= f =
    ValueFunc $ \value ->
      case func value of
        Pure a -> f a
        ValueFunc _ -> error "Expected no more arguments"

instance Applicative FromValue where
  (<*>) = ap
  pure = Pure

integer :: FromValue Integer
integer =
  ValueFunc $ \value ->
    case value of
      Prim (Integer i) -> Pure i
      _ -> error "Expected an integer"

bool :: FromValue Bool
bool =
  ValueFunc $ \value ->
    case value of
      Prim (Bool b) -> Pure b
      _ -> error "Expected a bool"
