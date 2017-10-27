module Lambda.PrettyPrint where

import Lambda.Calculus

instance Show Lambda where
  showsPrec _ (Var str) = showString str
  showsPrec p (Abs var body) =
    showParen (p > 0) (showString "λ" . showString var . showString "." . shows body)
  showsPrec p (App func arg) =
    showParen (p > 1) (showsPrec 1 func . showString " " . showsPrec 2 arg)
