module Typed.Lambda where

import Data.Functor.Foldable
import Data.Functor.Classes
import Text.Show.Deriving
import Text.Read.Deriving
import Data.Eq.Deriving

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map, (!?))

type Name = String

data Const = Type | Kind deriving (Show, Eq, Read)

data ExprF a
  = Const Const
  | FreeVar Name
  | BoundVar Int Name
  | a :@ a
  | Lambda Name a a
  | Pi Name a a
  deriving Functor

infixl :@

$(deriveShow1 ''ExprF)
$(deriveRead1 ''ExprF)

-- Checks for alpha-equivalence.
instance Eq1 ExprF where
  liftEq eq (Const a) (Const b) = a == b
  liftEq eq (FreeVar name1) (FreeVar name2) = name1 == name2
  liftEq eq (BoundVar i1 _) (BoundVar i2 _) = i1 == i2
  liftEq eq (f1 :@ a1) (f2 :@ a2) = eq f1 f2 && eq a1 a2
  liftEq eq (Lambda _ t1 b1) (Lambda _ t2 b2) = eq t1 t2 && eq b1 b2
  liftEq eq (Pi _ t1 b1) (Pi _ t2 b2) = eq t1 t2 && eq b1 b2
  liftEq _ _ _ = False

type Expr = Fix ExprF

weakHeadNormalForm :: Expr -> Expr
weakHeadNormalForm (Fix (func :@ arg)) =
  case weakHeadNormalForm func of
    Fix (Lambda name typ body) -> substitution arg body
    whnf -> Fix (whnf :@ arg)
weakHeadNormalForm other = other

substitution :: Expr -> Expr -> Expr
substitution replacement expr = shiftFree (-1) (replace (shiftFree 1 replacement) 0 expr)

shiftFree :: Int -> Expr -> Expr
shiftFree by expr = fold (shiftFreeAlg by) expr 0

shiftFreeAlg :: Int -> ExprF (Int -> Expr) -> Int -> Expr
shiftFreeAlg shift (BoundVar index name) freeIndex
  | index >= freeIndex = Fix (BoundVar (index + shift) name)
  | otherwise          = Fix (BoundVar index name)
shiftFreeAlg _ (Const c) _ = Fix (Const c)
shiftFreeAlg _ (FreeVar name) _ = Fix (FreeVar name)
shiftFreeAlg _ (func :@ arg) freeIndex = Fix (func freeIndex :@ arg freeIndex)
shiftFreeAlg shift (Lambda name shiftTyp shiftBody) freeIndex =
  Fix (Lambda name (shiftTyp freeIndex) (shiftBody (freeIndex + 1)))
shiftFreeAlg shift (Pi name shiftTyp shiftBody) freeIndex =
  Fix (Pi name (shiftTyp freeIndex) (shiftBody (freeIndex + 1)))


replace :: Expr -> Int -> Expr -> Expr
replace replacement index expr = fold replaceAlg expr index replacement

replaceAlg :: ExprF (Int -> Expr -> Expr) -> Int -> Expr -> Expr
replaceAlg (BoundVar index name) replaceIndex replacement
  | replaceIndex == index = replacement
  | otherwise             = Fix (BoundVar index name)
replaceAlg (Lambda name replaceType replaceBody) replaceIndex replacement =
  let typ = replaceType replaceIndex replacement
      body = replaceBody (replaceIndex + 1) (shiftFree 1 replacement)
   in Fix (Lambda name typ body)
replaceAlg (Pi name replaceType replaceBody) replaceIndex replacement =
 let typ = replaceType replaceIndex replacement
     body = replaceBody (replaceIndex + 1) (shiftFree 1 replacement)
  in Fix (Pi name typ body)
replaceAlg (func :@ arg) ind repl = Fix (func ind repl :@ arg ind repl)
replaceAlg (Const c) _ _ = Fix (Const c)
replaceAlg (FreeVar name) _ _ = Fix (FreeVar name)
