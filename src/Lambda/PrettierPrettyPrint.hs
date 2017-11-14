module Lambda.PrettierPrettyPrint
  ( pretty
  , render
  , prettyPrintLambda) where

import Lambda.Untyped
import Text.PrettyPrint.ANSI.Leijen

instance Pretty Lambda where
  pretty = prettyPrintLambda 0

prettyPrintLambda :: Int -> Lambda -> Doc
prettyPrintLambda _ (Var str) = string str
prettyPrintLambda p (Abs var body) =
  group (conditionalParen (p > 0) (bold (string "λ") <> string var <> string "." <> softline <> nest 2 (prettyPrintLambda 0 body)))
prettyPrintLambda p (App func arg) =
  group (conditionalParen (p > 1) (prettyPrintLambda 1 func <> softline <> align (nest 2 (prettyPrintLambda 2 arg))))

conditionalParen :: Bool -> Doc -> Doc
conditionalParen True = parens
conditionalParen False = id

render :: Int -> Doc -> String
render lineWidth = ($ "") . displayS . renderSmart 1 lineWidth
