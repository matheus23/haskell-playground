module SExpr.Types where

import Data.Functor.Foldable
import Text.Show.Deriving
import Text.Read.Deriving
import Data.Eq.Deriving

data SExprF e
  = Symbol String
  | List [e]
  deriving (Functor)

type SExpr = Fix SExprF

$(deriveShow1 ''SExprF)
$(deriveRead1 ''SExprF)
$(deriveEq1 ''SExprF)
