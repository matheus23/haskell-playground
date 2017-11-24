module Typed.PrettyPrint where

import Typed.Lambda
import Text.PrettyPrint.ANSI.Leijen
import Data.Functor.Foldable

prettyPrintExpr :: Expr -> Doc
prettyPrintExpr expr = group (para prettyPrintExprAlg expr False 0)

prettyPrintExprAlg :: ExprF (Expr, Bool -> Int -> Doc) -> Bool -> Int -> Doc
prettyPrintExprAlg exprF inTypeLevel precedence =
  let origExpr = Fix (fmap fst exprF)
  in case exprF of
    Const Type -> string "Type"
    Const Kind -> string "Kind"
    FreeVar name -> string name
    BoundVar i name -> string name

    (Pi "_" (_, ppArg) (_, ppResult)) ->
      parenWhen (precedence > 1)
        (ppArg inTypeLevel 2 <+> string "→" <+> ppResult inTypeLevel 1)

    Pi name (Fix (Const Type), _) (_, ppResult) ->
      parenWhen (precedence > 0)
        (string "∀" <+> string name <+> string "." <+> ppResult inTypeLevel 0)

    (Pi name (_, ppArg) (_, ppResult)) ->
      parenWhen (precedence > 0)
        (string "Π" <+> string name <+> string ":" <+> ppArg inTypeLevel 1 <+> string "." <+> ppResult inTypeLevel 0)

    (Lambda name (_, ppArgType) (Fix Lambda{}, ppBody)) ->
      wideParenWhen (precedence > 0)
        (string "λ" <+> string name <+> string ":" <+> ppArgType True 1 <> line
        <> string "." <+> ppBody inTypeLevel 0)

    (Lambda name (_, ppArgType) (_, ppBody)) ->
      wideParenWhen (precedence > 0)
        (string "λ" <+> string name <+> string ":" <+> ppArgType True 1 <> line
        <> string "." <+> group (align (ppBody inTypeLevel 0)))

    (_, ppFunc) :@ (_, ppArg) | inTypeLevel ->
      parenWhen (precedence > 2)
        (ppFunc inTypeLevel 2 <+> ppArg inTypeLevel 3)

    (_, ppFunc) :@ (_, ppArg) ->
      parenWhen (precedence > 2)
        (ppFunc inTypeLevel 2 <> line <> nest 2 (ppArg inTypeLevel 3))

parenWhen :: Bool -> Doc -> Doc
parenWhen True = group . parens
parenWhen False = id

wideParenWhen :: Bool -> Doc -> Doc
wideParenWhen True = enclose (string "( ") (string " )")
wideParenWhen False = id

render :: Int -> Doc -> String
render lineWidth = ($ "") . displayS . renderSmart 1 lineWidth
