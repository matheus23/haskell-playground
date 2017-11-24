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
  | TypeInt
  | TypeBool
  deriving Show

type ValueEnv = Map Int Value

interpretString :: String -> IO ()
interpretString str =
  let parsed = parseExpr str
   in case typeCheck stdTypes parsed of
        Right typ -> do
          putStrLn $ "Has type: " ++ render 40 (prettyPrintExpr typ)
          print $ interpret stdValues parsed
        Left error ->
          print $ prettyErr error

interpret :: (Name -> Maybe Value) -> Expr -> Value
interpret interpretFree expr = para (interpretAlg interpretFree) expr Map.empty

interpretAlg :: (Name -> Maybe Value) -> ExprF (Expr, ValueEnv -> Value) -> ValueEnv -> Value
interpretAlg interpretFree expr env =
  case expr of
    (FreeVar name) ->
      case interpretFree name of
        Just v -> v
        Nothing -> error ("Cannot interpret literal: " ++ show name)
    (BoundVar index _) -> lookupValueEnv env index
    (Const c) -> error "Cannot evaluate type system constants"
    Pi{} -> error "Cannot evaluate types"
    (Lambda name _ (_, interpretBody)) ->
      Fun $ \arg -> interpretBody (Map.insert 0 arg (Map.mapKeys (+ 1) env))
    ((origFunc, interpretFunc) :@ (origArg, interpretArg)) ->
      case interpretFunc env of
        Fun haskellFunction -> haskellFunction (interpretArg env)
        Prim primitive -> error ("Expected a function, but got primitive: " ++ show primitive ++ " In expression: "
          ++ show (Fix (origFunc :@ origArg)))


lookupValueEnv :: ValueEnv -> Int -> Value
lookupValueEnv env i =
  case env !? i of
    Just value -> value
    _ -> error ("Expression built incorrectly. Variable unbound: " ++ show i)

std :: Name -> (TypeCheck Expr, Maybe Value)
std "Int" = primitive "Type" (Pure (Prim TypeInt))
std "Bool" = primitive "Type" (Pure (Prim TypeBool))
std "+" = primitive "Int → Int → Int" $ Prim . Integer <$> ((+) <$> integer <*> integer)
std "-" = primitive "Int → Int → Int" $ Prim . Integer <$> ((-) <$> integer <*> integer)
std "*" = primitive "Int → Int → Int" $ Prim . Integer <$> ((*) <$> integer <*> integer)
std "//" = primitive "Int → Int → Int" $ Prim . Integer <$> (div <$> integer <*> integer)
std "negate" = primitive "Int → Int" $ Prim . Integer <$> (negate <$> integer)
std "&&" = primitive "Bool → Bool → Bool" $ Prim . Bool <$> ((&&) <$> bool <*> bool)
std "==" = primitive "Int → Int → Bool" $ Prim . Bool <$> ((==) <$> integer <*> integer)
std "if" = primitive "Bool → Int → Int → Int" $ Prim . Integer <$> (runIf <$> bool <*> integer <*> integer)
std literal =
  case listToMaybe (map fst ((reads :: ReadS Integer) literal)) of
    Just num -> primitive "Int" (pure $ Prim $ Integer num)
    Nothing -> (Left (InvalidLiteral literal), Nothing)



runIf :: Bool -> Integer -> Integer -> Integer
runIf True l _ = l
runIf False _ r = r

stdTypes :: Name -> TypeCheck Expr
stdTypes = fst . std

stdValues :: Name -> Maybe Value
stdValues = snd . std

primitive :: String -> FromValue Value -> (TypeCheck Expr, Maybe Value)
primitive typeName val =
  (return (parseExpr typeName), Just (makeValue val))

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
