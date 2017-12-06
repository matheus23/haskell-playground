module Typed.Explore where

import Typed.Lambda
import Typed.PrettyPrint
import Data.Functor.Foldable
import Text.PrettyPrint.ANSI.Leijen


data ExprContext a
  = WithAppFunc a
  | WithAppArg a
  | WithLambdaType Name a
  | WithLambdaBody Name a
  | WithPiArg Name a
  | WithPiResult Name a
  deriving Functor

data ExprZipper = ExprZipper Expr [ExprContext Expr]

reconstruct :: ExprZipper -> Expr
reconstruct (ExprZipper expr []) = expr
reconstruct (ExprZipper expr (context:restContext)) =
  reconstruct (ExprZipper (reconstructOnce context expr) restContext)

reconstructOnce :: ExprContext Expr -> Expr -> Expr
reconstructOnce context expr =
  case context of
    WithAppFunc func -> Fix (func :@ expr)
    WithAppArg arg -> Fix (expr :@ arg)
    WithLambdaType name typ -> Fix (Lambda name typ expr)
    WithLambdaBody name body -> Fix (Lambda name expr body)
    WithPiArg name typ -> Fix (Pi name typ expr)
    WithPiResult name body -> Fix (Pi name expr body)

focusTop :: Expr -> ExprZipper
focusTop expr = ExprZipper expr []

posDirections :: ExprZipper -> Maybe (ExprZipper, ExprZipper)
posDirections (ExprZipper (Fix Const{}) _) = Nothing
posDirections (ExprZipper (Fix FreeVar{}) _) = Nothing
posDirections (ExprZipper (Fix BoundVar{}) _) = Nothing
posDirections (ExprZipper (Fix (func :@ arg)) context) =
  Just (ExprZipper func (WithAppArg arg : context), ExprZipper arg (WithAppFunc func : context))
posDirections (ExprZipper (Fix (Lambda name typ body)) context) =
  Just (ExprZipper typ (WithLambdaBody name body : context), ExprZipper body (WithLambdaType name typ : context))
posDirections (ExprZipper (Fix (Pi name arg res)) context) =
  Just (ExprZipper arg (WithPiResult name res : context), ExprZipper res (WithPiArg name arg : context))

goLeft, goRight, goUp :: ExprZipper -> Maybe ExprZipper
goLeft = fmap fst . posDirections
goRight = fmap snd . posDirections
goUp (ExprZipper expr []) = Nothing
goUp (ExprZipper expr (context : restContext)) =
  Just (ExprZipper newExpr restContext)
  where
    newExpr =
      case context of
        WithAppArg arg -> Fix (expr :@ arg)
        WithAppFunc func -> Fix (func :@ expr)
        WithLambdaType name typ -> Fix (Lambda name typ expr)
        WithLambdaBody name body -> Fix (Lambda name expr body)
        WithPiArg name arg -> Fix (Pi name arg expr)
        WithPiResult name res -> Fix (Pi name expr res)

paraZipper :: (ExprF (Expr, a) -> a) -> (a -> a) -> ExprZipper -> a
paraZipper algebra onFocused (ExprZipper expr contexts) =
  unwrapContexts algebra contexts (expr, onFocused (para algebra expr))

unwrapContexts :: (ExprF (Expr, a) -> a) -> [ExprContext Expr] -> (Expr, a) -> a
unwrapContexts _ [] (_, a) = a
unwrapContexts algebra (context : restContext) (aExpr, a) =
  unwrapContexts algebra restContext $
    case context of
      WithAppFunc func -> (Fix (func :@ aExpr), algebra $ (func, para algebra func) :@ (aExpr, a))
      WithAppArg arg -> (Fix (aExpr :@ arg), algebra $ (aExpr, a) :@ (arg, para algebra arg))
      WithLambdaType name typ -> (Fix (Lambda name typ aExpr), algebra $ Lambda name (typ, para algebra typ) (aExpr, a))
      WithLambdaBody name body -> (Fix (Lambda name aExpr body), algebra $ Lambda name (aExpr, a) (body, para algebra body))
      WithPiArg name arg -> (Fix (Pi name arg aExpr), algebra $ Pi name (arg, para algebra arg) (aExpr, a))
      WithPiResult name res -> (Fix (Pi name aExpr res), algebra $ Pi name (aExpr, a) (res, para algebra res))

-- prettyPrintExprAlg :: ExprF (Expr, Bool -> Int -> Doc) -> Bool -> Int -> Doc

prettyPrintExprZipper :: ExprZipper -> Doc
prettyPrintExprZipper zipper = paraZipper prettyPrintExprAlg handleFocused zipper False 0
  where
    handleFocused getFocused inType precedence =
      bold (getFocused inType precedence)

printZipper :: ExprZipper -> IO ()
printZipper = putStrLn . showZipper

showZipper :: ExprZipper -> String
showZipper = render 60 . prettyPrintExprZipper

instance Show ExprZipper where
  show = showZipper
