module Typed.NormalizationByEvaluation where

import qualified Data.Map as Map
import Data.Map ((!?), Map)
import Data.Maybe
import Data.Functor.Foldable

import Typed.Lambda
import Typed.Interpreter
import Typed.PrettyPrint

data ReifyableValue
  = RPrim Primitive
  | Neutral Neutral
  | FunLam Name ReifyableValue (ReifyableValue -> ReifyableValue)
  | FunPi Name ReifyableValue (ReifyableValue -> ReifyableValue)

data Neutral
  = SynFreeVar Name
  | SynBoundVar Int Name
  | SynConst Const
  | SynApp Neutral ReifyableValue

instance Show ReifyableValue where
  show = render 60 . prettyPrintExpr . reify


type ReifyableValueEnv = DeBrujinLookup ReifyableValue

normalize :: Expr -> Expr
normalize = reify . eval

eval :: Expr -> ReifyableValue
eval = evalInEnv Map.empty

evalInEnv :: ReifyableValueEnv -> Expr -> ReifyableValue
evalInEnv env expr =
  case expr of
    Fix (Const c) -> Neutral (SynConst c)
    Fix (FreeVar f) -> fromMaybe (Neutral (SynFreeVar f)) (evalLiteral f)
    Fix (BoundVar index name) -> fromMaybe (Neutral (SynBoundVar index name)) (env !? index)

    Fix (func :@ arg) ->
      case (evalInEnv env func, evalInEnv env arg) of
        (FunLam name typValue fun, evaledArg) -> fun evaledArg
        (Neutral neutralFunc, evaledArg) -> Neutral (SynApp neutralFunc evaledArg)
        (notFunc, evaledArg) -> error ("Expected function, but got something else. In expression: " ++ show expr)

    Fix (Lambda name typ body) ->
      FunLam name (evalInEnv env typ) (evalAbstractionBody env body)

    Fix (Pi name typ body) ->
      FunPi name (evalInEnv env typ) (evalAbstractionBody env body)

evalAbstractionBody :: ReifyableValueEnv -> Expr -> (ReifyableValue -> ReifyableValue)
evalAbstractionBody env body arg =
    (evalInEnv
      (insertFromAbstraction arg env)
      body)

shiftFreeValue :: Int -> Int -> ReifyableValue -> ReifyableValue
shiftFreeValue limit shift value =
  case value of
    Neutral n -> Neutral (shiftFreeNeutral limit shift n)

    FunLam name typ bodyValue ->
      FunLam name (shiftFreeValue limit shift typ) (shiftFreeValue (limit + 1) shift . bodyValue)

    FunPi name typ bodyValue ->
      FunPi name (shiftFreeValue limit shift typ) (shiftFreeValue (limit + 1) shift . bodyValue)

    _ -> value

shiftFreeNeutral :: Int -> Int -> Neutral -> Neutral
shiftFreeNeutral limit shift (SynApp func val) =
  SynApp (shiftFreeNeutral limit shift func) (shiftFreeValue limit shift val)
shiftFreeNeutral limit shift (SynBoundVar index name)
  | index >= limit = SynBoundVar (shift + index) name
  | otherwise = SynBoundVar index name
shiftFreeNeutral limit _ other = other

evalLiteral :: Name -> Maybe ReifyableValue
evalLiteral "Int" = Just (RPrim TypeInt)
evalLiteral "+" =
  Just
    (FunLam "lhs" (RPrim TypeInt) $ \lhsValue ->
      FunLam "rhs" (RPrim TypeInt) $ \rhsValue ->
        case (lhsValue, rhsValue) of
          (RPrim (Integer lhs), RPrim (Integer rhs)) -> RPrim (Integer (lhs + rhs))
          _ ->
            Neutral
              (SynApp
                (SynApp
                  (SynFreeVar "+")
                  lhsValue)
                rhsValue))
evalLiteral literal =
  case listToMaybe (map fst ((reads :: ReadS Integer) literal)) of
    Just num -> Just (RPrim (Integer num))
    Nothing -> Nothing


reify :: ReifyableValue -> Expr
reify value =
  case value of
    RPrim prim -> reifyPrim prim
    Neutral neutral -> reifyNeutral neutral
    FunLam name typ getBody ->
      Fix (Lambda name (reify typ) (reify (getBody (Neutral (SynBoundVar 0 name)))))
    FunPi name typ getBody ->
      Fix (Pi name (reify typ) (reify (getBody (Neutral (SynBoundVar 0 name)))))

reifyPrim :: Primitive -> Expr
reifyPrim (Integer i) = Fix (FreeVar (show i))
reifyPrim (Bool True) = Fix (FreeVar "true")
reifyPrim (Bool False) = Fix (FreeVar "false")
reifyPrim TypeInt = Fix (FreeVar "Int")
reifyPrim TypeBool = Fix (FreeVar "Bool")

reifyNeutral :: Neutral -> Expr
reifyNeutral neutral =
  case neutral of
    SynFreeVar name -> Fix (FreeVar name)
    SynBoundVar index name -> Fix (BoundVar index name)
    SynConst c -> Fix (Const c)
    SynApp neutral val -> Fix (reifyNeutral neutral :@ reify val)
