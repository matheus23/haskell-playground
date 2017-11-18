module Typed.TypeCheck where

import Data.Functor.Foldable
import qualified Data.Map as Map
import Data.Map (Map, (!?))

import Typed.Lambda
import Typed.PrettyPrint

type TypeCheck = Either TypeCheckError

data TypeCheckError
  = UnkownBoundVar Int Name (Map Int Type)
  | ExpectedFunctionType Type
  | ArgTypeMismatch Type Type
  | InvalidLiteral Name
  deriving (Show, Eq)


typeCheck :: (Name -> TypeCheck Type) -> IndexedExpr -> TypeCheck Type
typeCheck typeCheckFree expr = fold (typeCheckAlg typeCheckFree) expr Map.empty

typeCheckAlg :: (Name -> TypeCheck Type) -> IndexedExprF (Map Int Type -> TypeCheck Type) -> Map Int Type -> TypeCheck Type
typeCheckAlg _ (BoundVar i name) env = lookupEnv env i name
typeCheckAlg _ (Expr (getFuncType :@ getArgType)) env = do
  funcType <- getFuncType env
  (funcArgType, funcResultType) <- expectArrowType funcType
  argType <- getArgType env
  expectMatchingTypes funcArgType argType
  return funcResultType
typeCheckAlg _ (Expr (Abs name typ getBodyType)) env = do
  let newEnv = Map.insert 0 typ (Map.mapKeys (+ 1) env)
  bodyType <- getBodyType newEnv
  return (Fix (typ :-> bodyType))
typeCheckAlg typeCheckFree (Expr (Var name)) env = typeCheckFree name

lookupEnv :: Map Int Type -> Int -> Name -> TypeCheck Type
lookupEnv env i name =
  case env !? i of
    Nothing -> Left (UnkownBoundVar i name env)
    Just typ -> return typ

expectArrowType :: Type -> TypeCheck (Type, Type)
expectArrowType (Fix (arg :-> res)) = return (arg, res)
expectArrowType t = Left (ExpectedFunctionType t)

expectMatchingTypes :: Type -> Type -> TypeCheck ()
expectMatchingTypes a b =
  if a == b then return () else Left (ArgTypeMismatch a b)

prettyErr :: TypeCheckError -> String
prettyErr (UnkownBoundVar index name env) = "Unknown bound variable " ++ show name ++ " (index " ++ show index ++ ")"
prettyErr (ExpectedFunctionType typ) = "Expected function, but got " ++ render 40 (prettyPrintType typ) ++ " instead"
prettyErr (ArgTypeMismatch funcType argType) =
  "Expected argument of type "
  ++ render 40 (prettyPrintType funcType)
  ++ ", but actually got an argument of type "
  ++ render 40 (prettyPrintType argType)
  ++ " instead"
prettyErr (InvalidLiteral name) = "Cannot identify literal " ++ show name
