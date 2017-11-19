module Typed.Lambda where

import Data.Functor.Foldable
import Text.Show.Deriving
import Text.Read.Deriving
import Data.Eq.Deriving

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map, (!?))

type Name = String

data ExprF a
  = Type
  | Kind
  | FreeVar Name
  | BoundVar Int Name
  | a :@ a
  | Lambda Name (ExprF a) a
  | Pi Name (ExprF a) a
  deriving Functor

infixl :@

$(deriveShow1 ''ExprF)
$(deriveRead1 ''ExprF)
$(deriveEq1 ''ExprF)

type Expr = Fix ExprF

{-
computeIndexedAlg :: ExprF (Map String Int -> IndexedExpr) -> Map String Int -> IndexedExpr
computeIndexedAlg (Var name) environment =
  case environment !? name of
    Just index -> Fix (BoundVar index name)
    Nothing    -> Fix (Expr (Var name))
computeIndexedAlg (getFunc :@ getArg) environment =
  Fix (Expr (getFunc environment :@ getArg environment))
computeIndexedAlg (Abs name typ getBody) environment =
  Fix (Expr (Abs name typ (getBody (Map.insert name 0 (fmap (+ 1) environment)))))

computeIndexed :: Expr -> IndexedExpr
computeIndexed expr = fold computeIndexedAlg expr Map.empty
-}
