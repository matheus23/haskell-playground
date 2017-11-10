module Lambda.PrettierPrettyPrint where

import Lambda.Untyped
import Text.PrettyPrint.ANSI.Leijen

prettyPrintLambda :: Lambda -> Doc
prettyPrintLambda _ (Var str) = string str
prettyPrintLambda p (Abs var body) =
  conditionalParen (p > 0) (bold (string "λ") <> string var <> string "." <+> prettyPrintLambda body)
prettyPrintLambda p (App func arg) =
  conditionalParen (p > 1) (prettyPrintLambda 1 func <+> prettyPrintLambda 2 arg)

conditionalParen :: Bool -> Doc -> Doc
conditionalParen True = paren
conditionalParen False = id
