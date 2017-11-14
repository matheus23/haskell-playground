module Logic.PrettyPrinter where

import Logic.Propositional
import Data.Functor.Foldable
import Data.Functor.Classes

prettyPrintExprAlg :: ExprF (Int -> ShowS) -> Int -> ShowS
prettyPrintExprAlg Zero _ = showString "0"
prettyPrintExprAlg One _ = showString "1"
prettyPrintExprAlg (Var name) _ = showString name
prettyPrintExprAlg (Not expr) p = showParen (p > 5) $ showString "¬" . expr 5
prettyPrintExprAlg (Op operator lhs rhs) p =
  showParen (p > (prec - 1))
    (lhs prec . showString (prettyPrintOp operator) . rhs (prec - 1))
  where
    prec = precedence operator

prettyPrintExpr :: Expr -> String
prettyPrintExpr expr = fold prettyPrintExprAlg expr 0 "" -- 0 initial precedence, "" for printing ShowS

precedence :: BiOperator -> Int
precedence And = 4
precedence Or = 3
precedence Equiv = 2
precedence Imply = 1

prettyPrintOp :: BiOperator -> String
prettyPrintOp And = " ∧ "
prettyPrintOp Or = " ∨ "
prettyPrintOp Equiv = " ⇔ "
prettyPrintOp Imply = " ⇒ "
