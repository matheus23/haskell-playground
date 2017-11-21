module Typed.TypeCheck where

import Data.Functor.Foldable
import qualified Data.Map as Map
import Data.Map (Map, (!?))

import Typed.Lambda
import Typed.PrettyPrint

type TypeCheck = Either TypeCheckError

type Env = Map Int Expr

data TypeCheckError
  = UnkownBoundVar Int Name Env
  | ExpectedPiType Expr
  | ArgTypeMismatch Expr Expr
  | InvalidLiteral Name
  | TypeOfKind
  | ExpectedTypeOrKind Expr
  | IllegalDependency Const Const
  deriving (Show, Eq)

resultingDependency :: Const -> Const -> TypeCheck Const
resultingDependency Type Kind = return Kind -- value results in type, i.e. "Vec n", where n is a natural number
resultingDependency Type Type = return Type -- value depends on another value. Just ordinary functions
resultingDependency Kind Kind = return Kind -- type results in type. For example "List" (* -> *) or (Type -> Type) is allowed
-- We disallow a value depending on a type. For example:
-- numBits : Type -> Int
-- numBits t =
--   case t of
--     Int -> 64
--     Bool -> 1
--     _ -> -1
resultingDependency Kind Type = Left (IllegalDependency Kind Type)

typeCheck :: (Name -> TypeCheck Expr) -> Expr -> TypeCheck Expr
typeCheck typeCheckFree expr = para (typeCheckAlg typeCheckFree) expr Map.empty

typeCheckAlg :: (Name -> TypeCheck Expr) -> ExprF (Expr, Env -> TypeCheck Expr) -> Env -> TypeCheck Expr
typeCheckAlg typeOfFree expr env =
  case expr of
    (FreeVar name) -> typeOfFree name
    (BoundVar index name) -> lookupEnv env index name
    (Const Type) -> return (Fix (Const Kind))
    (Const Kind) -> Left TypeOfKind
    (_, typeOfFunc) :@ (_, typeOfArg) -> do
      (Pi name typ resType) <- expectPiType =<< weakHeadNormalForm <$> typeOfFunc env
      argType <- typeOfArg env
      return (substitution argType resType)
    (Lambda name (argType, typeCheckArgType) (_, typeOfBody)) -> do
      _ <- typeCheckArgType env
      bodyType <- typeOfBody (insertEnv argType env)
      return (Fix (Pi name argType bodyType))
    (Pi name (argType, typeOfArg) (_, typeOfRes)) -> do
      argTypeOrKind <- expectTypeOrKind =<< weakHeadNormalForm <$> typeOfArg env
      let env' = insertEnv (Fix (Const argTypeOrKind)) env
      resTypeOrKind <- expectTypeOrKind =<< weakHeadNormalForm <$> typeOfRes env'
      Fix . Const <$> resultingDependency argTypeOrKind resTypeOrKind

insertEnv :: Expr -> Env -> Env
insertEnv expr = Map.insert 0 expr . Map.mapKeys (+ 1)

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
prettyErr (ArgTypeMismatch funcType argType) =
  "Expected argument of type "
  ++ show (render 40 (prettyPrintExpr funcType))
  ++ ", but actually got an argument of type "
  ++ show (render 40 (prettyPrintExpr argType))
  ++ " instead"
prettyErr (InvalidLiteral name) = "Cannot identify literal " ++ show name

printTC :: TypeCheck Expr -> IO ()
printTC (Left err) = putStrLn (prettyErr err)
printTC (Right typ) = putStrLn (render 40 (prettyPrintExpr typ))
