module Lambda.Expressions where

import Lambda.Untyped
import Lambda.Parser
import Lambda.PrettyPrint
import Lambda.Interpreter

import Data.Maybe (fromMaybe)

class LambdaValue a where
  interpretMaybe :: Lambda -> Maybe a
  interpret :: Lambda -> a
  toLambda :: a -> Lambda

  interpret expr = fromMaybe (error ("Lambda cannot be interpreted: " ++ show expr)) (interpretMaybe expr)
  interpretMaybe = Just . interpret

-- Booleans

instance LambdaValue Bool where
  interpretMaybe expr =
    case normalForm (expr `App` "true" `App` "false") of
      Var "true" -> Just True
      Var "false" -> Just False
      _ -> Nothing

  toLambda True = boolTrue
  toLambda False = boolFalse

boolTrue :: Lambda
boolTrue = "λt. λf. t"

boolFalse :: Lambda
boolFalse = "λt. λf. f"

boolNot :: Lambda
boolNot = "λbool. λt. λf. bool f t"

lamLet :: String -> Lambda -> Lambda -> Lambda
lamLet name letLam inLam =
  normalForm (App (Abs name inLam) letLam)

boolOr :: Lambda
boolOr =
  lamLet "true" boolTrue $
  lamLet "false" boolFalse
  "λboolL. λboolR. boolL true boolR"

boolAnd :: Lambda
boolAnd =
  lamLet "true" boolTrue $
  lamLet "false" boolFalse
  "λboolL. λboolR. boolL boolR false"

boolEq :: Lambda
boolEq =
  lamLet "true" boolTrue $
  lamLet "false" boolFalse $
  lamLet "not" boolNot
  "λboolL. λboolR. boolL boolR (not boolR)"

-- Positive Integers:

churchNat :: Int -> Lambda
churchNat n = Abs "0" (Abs "+1" (iterate (App (Var "+1")) (Var "0") !! n))

instance LambdaValue Int where
  interpret churchNumeral =
    case executeStdLib (churchNumeral `App` "0" `App` "+1") of
      Primitive (Number n) -> fromIntegral n
      other -> error ("Lambda expression did not evaluate to number, but instead to: " ++ show other)

  toLambda = churchNat

natPlus :: Lambda
natPlus = "λnatA. λnatB. λ0. λ+1. natA (natB 0 +1) +1"
