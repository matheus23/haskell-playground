module Typed.PrettyPrint where

import Typed.Lambda
import Text.PrettyPrint.ANSI.Leijen
import Data.Functor.Foldable

prettyPrintExpr :: Expr -> Doc
prettyPrintExpr typ = para prettyPrintExprAlg typ 0

prettyPrintExprAlg :: ExprF (Expr, Int -> Doc) -> Int -> Doc
prettyPrintExprAlg exprF precedence =
  let origExpr = Fix (fmap fst exprF)
  in case exprF of
    Const Type -> string "Type"
    Const Kind -> string "Kind"
    FreeVar name -> string name
    BoundVar i name -> string name

    (Pi "_" (_, ppArg) (_, ppResult)) ->
      parenWhen (precedence > 1)
        (ppArg 2 <+> string "→" <+> ppResult 1)

    Pi name (Fix (Const Type), _) (_, ppResult) ->
      parenWhen (precedence > 0)
        (string "∀" <+> string name <+> string "." <+> ppResult 0)

    (Pi name (_, ppArg) (_, ppResult)) ->
      parenWhen (precedence > 0)
        (string "Π" <+> string name <+> string ":" <+> ppArg 1 <+> string "." <+> ppResult 0)

    (Lambda name (_, ppArgType) (Fix Lambda{}, ppBody)) ->
      wideParenWhen (precedence > 0)
        (string "λ" <+> string name <+> string ":" <+> ppArgType 1 <> line
        <> string "." <+> ppBody 0)

    (Lambda name (_, ppArgType) (_, ppBody)) ->
      wideParenWhen (precedence > 0)
        (string "λ" <+> string name <+> string ":" <+> ppArgType 1 <> line
        <> string "." <+> nest 2 (ppBody 0))

    (_, ppFunc) :@ (_, ppArg) ->
      parenWhen (precedence > 2)
        (ppFunc 2 <> softline <> ppArg 3)

parenWhen :: Bool -> Doc -> Doc
parenWhen True = group . parens
parenWhen False = id

wideParenWhen :: Bool -> Doc -> Doc
wideParenWhen True = enclose (string "( ") (string " )")
wideParenWhen False = id

render :: Int -> Doc -> String
render lineWidth = ($ "") . displayS . renderSmart 1 lineWidth
