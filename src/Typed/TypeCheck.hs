module Typed.TypeCheck where

import Data.Functor.Foldable
import qualified Data.Map as Map
import Data.Map (Map, (!?))
import Control.Monad

import Typed.Lambda
import Typed.PrettyPrint

type TypeCheck = Either TypeCheckError

type Env = Map Int Expr

data TypeCheckError
  = UnkownBoundVar Int Name Env
  | ExpectedPiType Expr
  | ArgTypeMismatch Expr Expr Expr
  | InvalidLiteral Name
  | TypeOfKind
  | ExpectedTypeOrKind Expr
  | IllegalDependency Expr Const Const
  deriving (Show, Eq)

-- lets not allow mixing values and types for now:
resultingDependency :: Expr -> Const -> Const -> TypeCheck Const
--resultingDependency expr Type Kind = return Kind -- value results in type, i.e. "Vec n", where n is a natural number
resultingDependency expr Type Kind = Left (IllegalDependency expr Type Kind) -- we disallow it for now. With that we get System Fω
resultingDependency expr Type Type = return Type -- value depends on another value. Just ordinary functions
resultingDependency expr Kind Kind = return Kind -- type results in type (type operators). For example "List" (* -> *) or (Type -> Type) is allowed
resultingDependency expr Kind Type = return Type -- This allows polymorphism (i.e. ∀ (a : Type) . a), left side has type Kind, right side Type

typeCheck :: (Name -> TypeCheck Expr) -> Expr -> TypeCheck Expr
typeCheck typeCheckFree expr = para (typeCheckAlg typeCheckFree) expr Map.empty

typeCheckAlg :: (Name -> TypeCheck Expr) -> ExprF (Expr, Env -> TypeCheck Expr) -> Env -> TypeCheck Expr
typeCheckAlg typeOfFree expr env =
  case expr of
    (FreeVar name) -> typeOfFree name
    (BoundVar index name) -> lookupEnv env index name
    (Const Type) -> return (Fix (Const Kind))
    (Const Kind) -> Left TypeOfKind

    (func, typeOfFunc) :@ (arg, typeOfArg) -> do
      (Pi name expectedArgType resType) <- expectPiType =<< weakHeadNormalForm <$> typeOfFunc env
      actualArgType <- typeOfArg env
      when (actualArgType /= expectedArgType)
        (Left (ArgTypeMismatch (Fix (func :@ arg)) expectedArgType actualArgType))
      return (substitution arg resType)

    (Lambda name (argType, typeCheckArgType) (_, typeOfBody)) -> do
      _ <- typeCheckArgType env
      bodyType <- typeOfBody (insertEnv argType env)
      return (Fix (Pi name argType bodyType))

    (Pi name (argType, typeOfArg) (resType, typeOfRes)) -> do
      argTypeOrKind <- expectTypeOrKind =<< weakHeadNormalForm <$> typeOfArg env
      let env' = insertEnv argType env
      resTypeOrKind <- expectTypeOrKind =<< weakHeadNormalForm <$> typeOfRes env'
      Fix . Const <$> resultingDependency (Fix (fst <$> expr)) argTypeOrKind resTypeOrKind

insertEnv :: Expr -> Env -> Env
insertEnv expr = Map.insert 0 (shiftFree 1 expr) . Map.mapKeys (+ 1) . fmap (shiftFree 1)

expectPiType :: Expr -> TypeCheck (ExprF Expr)
expectPiType (Fix pi@Pi{}) = return pi
expectPiType other = Left (ExpectedPiType other)

expectTypeOrKind :: Expr -> TypeCheck Const
expectTypeOrKind (Fix (Const c)) = return c
expectTypeOrKind other = Left (ExpectedTypeOrKind other)

lookupEnv :: Map Int Expr -> Int -> Name -> TypeCheck Expr
lookupEnv env i name =
  case env !? i of
    Nothing -> Left (UnkownBoundVar i name env)
    Just typ -> return typ

prettyErr :: TypeCheckError -> String
prettyErr (UnkownBoundVar index name env) = "Unknown bound variable " ++ show name ++ " (index " ++ show index ++ ")"
prettyErr (ExpectedPiType typ) = "Expected function, but got " ++ show (render 40 (prettyPrintExpr typ)) ++ " instead"
prettyErr (ArgTypeMismatch expr funcType argType) =
  " Function expects: "
  ++ show funcType
  ++ "\nArgument has type: "
  ++ show argType
  ++ "\nin Expression: "
  ++ show expr
prettyErr (InvalidLiteral name) = "Cannot identify literal " ++ show name
prettyErr TypeOfKind = "Cannot get the type of a Kind"
prettyErr (ExpectedTypeOrKind expr) = "Expected something that is a type or a kind: " ++ show expr
prettyErr (IllegalDependency expr left right) = "This type system does not allow dependencies from " ++ show left ++ " to " ++ show right
  ++ "\nin expression: " ++ show expr



printTC :: TypeCheck Expr -> IO ()
printTC (Left err) = putStrLn (prettyErr err)
printTC (Right typ) = putStrLn (render 40 (prettyPrintExpr typ))
