module Logic.PrettyPrinter where

import Logic.Propositional

instance Show Expr where
  showsPrec _ Zero = showString "0"
  showsPrec _ One = showString "1"
  showsPrec _ (Var name) = showString name
  showsPrec p (Not expr) = showParen (p > 5) $
    showString "¬" . showsPrec 5 expr
  showsPrec p (Op operator lhs rhs) = showParen (p > (prec - 1)) $
    showsPrec prec lhs . showString (prettyPrintOp operator) . showsPrec (prec - 1) rhs
    where prec = precedence operator

parens :: String -> String
parens str = "(" ++ str ++ ")"

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
