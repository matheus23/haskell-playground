{-# LANGUAGE DeriveFunctor #-}
module Lambda.Interpreter where

import Lambda.Untyped
import Lambda.PrettyPrint

import qualified Data.Map as Map
import Data.Map ((!), Map)
import Control.Monad
import Data.Maybe
import Data.Monoid

type Env = Map Name Value

data Primitive
  = Number Int
  | Boolean Bool

data Value
  = Primitive Primitive
  | Fun Representation (Value -> Value)

data Representation
  = FromPrimitive String [Value]
  | FromLambda Lambda

instance Show Primitive where
  show (Number n) = show n
  show (Boolean b) = show b

instance Show Representation where
  show (FromPrimitive name []) = name
  show (FromPrimitive name applied) = "(" ++ name ++ concatMap ((" " ++) . show) applied ++ ")"
  show (FromLambda expr) = show expr

instance Show Value where
  show (Primitive prim) = show prim
  show (Fun representation _) =
    show representation

executeWith :: Env -> Lambda -> Value
executeWith environment (Var name) =
  fromJust (getFirst (First (Map.lookup name environment) `mappend` First (parseLiteral name)))
executeWith environment (Abs argName body) =
  Fun (FromLambda (Abs argName body)) $ \argValue ->
    executeWith (Map.insert argName argValue environment) body
executeWith environment (App func arg) =
  let funcValue = executeWith environment func
      argValue  = executeWith environment arg
   in case funcValue of
        Fun _ apply -> apply argValue
        other -> error ("Expected function, but got: " ++ show other)

parseLiteral :: Name -> Maybe Value
parseLiteral "True" = Just (Primitive (Boolean True))
parseLiteral "False" = Just (Primitive (Boolean False))
parseLiteral numberLit =
  Primitive . Number <$> listToMaybe (map fst ((reads :: ReadS Int) numberLit))

executeStdLib :: Lambda -> Value
executeStdLib = executeWith stdLib

stdLib :: Env
stdLib =
  Map.fromList
    [ primitiveFunc "+1" (Primitive . Number . (+ 1) <$> number)
    , primitiveFunc "+" (Primitive . Number <$> ((+) <$> number <*> number))
    , primitiveFunc "-" (Primitive . Number <$> ((-) <$> number <*> number))
    , primitiveFunc "!" (Primitive . Boolean . not <$> boolean)
    , primitiveFunc "|" (Primitive . Boolean <$> ((||) <$> boolean <*> boolean))
    , primitiveFunc "&" (Primitive . Boolean <$> ((&&) <$> boolean <*> boolean))
    , primitiveFunc "=" (Primitive . Boolean <$> ((==) <$> number <*> number))
    , primitiveFunc ">" (Primitive . Boolean <$> ((>) <$> number <*> number))
    , primitiveFunc "<" (Primitive . Boolean <$> ((<) <$> number <*> number))
    , primitiveFunc ">=" (Primitive . Boolean <$> ((>=) <$> number <*> number))
    , primitiveFunc "<=" (Primitive . Boolean <$> ((<=) <$> number <*> number))
    ]

data ReadPrimitive a
  = PrimFun (Value -> ReadPrimitive a)
  | Pure a
  deriving (Functor)

instance Applicative ReadPrimitive where
  (<*>) = ap
  pure = Pure

instance Monad ReadPrimitive where
  PrimFun fun >>= f =
    PrimFun $ \val ->
      case fun val of
        Pure a -> f a
        other -> error "Expecting no more arguments."

number :: ReadPrimitive Int
number = PrimFun $ \val ->
  case val of
    Primitive (Number n) -> Pure n
    other -> error ("Expected a number, but got: " ++ show other)

boolean :: ReadPrimitive Bool
boolean = PrimFun $ \val ->
  case val of
    Primitive (Boolean n) -> Pure n
    other -> error ("Expected a boolean, but got: " ++ show other)


primitiveFunc :: String -> ReadPrimitive Value -> (String, Value)
primitiveFunc name readPrimitive = (name, makeFunc name [] readPrimitive)

makeFunc :: String -> [Value] -> ReadPrimitive Value -> Value
makeFunc name applied (Pure val) = val
makeFunc name applied (PrimFun f) =
  Fun (FromPrimitive name applied) $ \val ->
    makeFunc name (applied ++ [val]) (f val)
