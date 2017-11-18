module Typed.PrettyPrint where

import Typed.Lambda
import Text.PrettyPrint.ANSI.Leijen
import Data.Functor.Foldable

prettyPrintType :: Type -> Doc
prettyPrintType typ = group (fold prettyPrintTypeAlg typ False)

prettyPrintTypeAlg :: TypeF (Bool -> Doc) -> Bool -> Doc
prettyPrintTypeAlg (TypeVar name) _ = string name
prettyPrintTypeAlg (arg :-> res) leftOfArrow =
  (if leftOfArrow then group . parens else id)
    (arg True <> line <> string "->" <+> res False)

render :: Int -> Doc -> String
render lineWidth = ($ "") . displayS . renderSmart 1 lineWidth
