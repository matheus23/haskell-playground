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



data TypeF a
  = FreeType Name
  | TypeVar Int Name
  | a :-> a
  | ForallType Name a
  deriving Functor

infixr :->

$(deriveShow1 ''TypeF)
$(deriveRead1 ''TypeF)
$(deriveEq1 ''TypeF)

type Type = Fix TypeF


data ExprF a
  = FreeVar Name
  | Var Int Name
  | a :@ a
  | Abs Name Type a
  | ForallAbs Name a
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
