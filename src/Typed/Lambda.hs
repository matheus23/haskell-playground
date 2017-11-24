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
import Data.Maybe

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

strongStep :: Expr -> Maybe Expr
strongStep (Fix (Fix (Lambda name typ body) :@ arg)) = Just (substitution arg body)
strongStep (Fix (func :@ arg)) =
  case (strongStep func, strongStep arg) of
    (Nothing, Nothing) -> Nothing
    (func', arg') -> Just (Fix (fromMaybe func func' :@ fromMaybe arg arg'))
strongStep (Fix (Lambda name typ body)) =
  case (strongStep typ, strongStep body) of
    (Nothing, Nothing) -> Nothing
    (typ', body') -> Just (Fix (Lambda name (fromMaybe typ typ') (fromMaybe body body')))
strongStep (Fix (Pi name typ body)) =
  case (strongStep typ, strongStep body) of
    (Nothing, Nothing) -> Nothing
    (typ', body') -> Just (Fix (Pi name (fromMaybe typ typ') (fromMaybe body body')))
strongStep other = Nothing

weakHeadStep :: Expr -> Maybe Expr
weakHeadStep (Fix (Fix (Lambda name typ body) :@ arg)) = Just (substitution arg body)
weakHeadStep _ = Nothing

weakHeadNormalForm :: Expr -> Expr
weakHeadNormalForm = normalForm weakHeadStep

strongNormalForm :: Expr -> Expr
strongNormalForm = normalForm strongStep

normalForm :: (Expr -> Maybe Expr) -> Expr -> Expr
normalForm reducer expr =
  case reducer expr of
    Just next -> normalForm reducer next
    Nothing -> expr

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

type DeBrujinLookup a = Map Int a

insertFromAbstraction :: a -> DeBrujinLookup a -> DeBrujinLookup a
insertFromAbstraction value = Map.insert 0 value . Map.mapKeys (+ 1)
