module Typed.PrettyPrint where

import Typed.Lambda
import Text.PrettyPrint.ANSI.Leijen
import Data.Functor.Foldable

prettyPrintExpr :: Expr -> Doc
prettyPrintExpr typ = group (para prettyPrintExprAlg typ False)

prettyPrintExprAlg :: ExprF (Expr, Bool -> Doc) -> Bool -> Doc
prettyPrintExprAlg (Const Type) _ = string "Type"
prettyPrintExprAlg (Const Kind) _ = string "Kind"
prettyPrintExprAlg (FreeVar name) _ = string name
prettyPrintExprAlg (BoundVar _ name) _ = string name
prettyPrintExprAlg (Pi name (Fix (Const Type), _) (_, body)) _ =
  string "∀" <+> string name <+> string "."
    <> softline <> body False
prettyPrintExprAlg (Pi "_" (_, arg) (_, res)) leftOfArrow =
  (if leftOfArrow then group . parens else id)
    (arg True <> line <> string "->" <+> res False)
prettyPrintExprAlg ((_, func) :@ (_, arg)) _ = func False <+> func False
prettyPrintExprAlg (Lambda name (_, typ) (_, body)) _ =
  group
    (string "λ" <+> string name <+> string ":" <+> typ False <+> string "."
      <> softline <> body False)
prettyPrintExprAlg (Pi name (_, typ) (_, body)) _ =
  group
    (string "Π" <+> string name <+> string ":" <+> typ False <+> string "."
      <> softline <> body False)


render :: Int -> Doc -> String
render lineWidth = ($ "") . displayS . renderSmart 1 lineWidth
