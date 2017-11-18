module SExpr.PrettyPrint where

import Text.PrettyPrint.ANSI.Leijen
import Data.List (intersperse)
import Data.Functor.Foldable

import SExpr.Types

prettyPrintAlgebra :: SExprF Doc -> Doc
prettyPrintAlgebra (Symbol str) = string str
prettyPrintAlgebra (List []) = string "()"
prettyPrintAlgebra (List (firstDoc:docs)) =
  string "("
  <> firstDoc
  <> softline
  <> group (nest 2 (mconcat (intersperse softline docs)))
  <> string ")"

prettyPrintSExpr :: SExpr -> Doc
prettyPrintSExpr = fold prettyPrintAlgebra

render :: Int -> Doc -> String
render lineWidth = ($ "") . displayS . renderSmart 1 lineWidth
