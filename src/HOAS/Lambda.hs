{-# LANGUAGE RankNTypes, RecordWildCards #-}
module HOAS.Lambda where

import Data.Functor.Foldable

data Expr expr
  = Expr
  { lam :: String -> (expr -> expr) -> expr
  , app :: expr -> expr -> expr
  }

data FirstOrder
  = Var String Int
  | App FirstOrder FirstOrder
  | Lam String FirstOrder
  deriving (Show, Eq, Read)

data NormalForm
  = Neutral Neutral
  | NLam String (NormalForm -> NormalForm)

data Neutral
  = NVar String Int
  | NApp Neutral NormalForm

true :: Expr expr -> expr
true Expr{..} =
  lam "t" (\t -> lam "f" (\f -> t))

notL :: Expr expr -> expr
notL Expr{..} =
  lam "bool" (\bool -> lam "t" (\t -> lam "f" (\f -> bool `app` f `app` t)))

false :: Expr expr -> expr
false l@Expr{..} = notL l `app` true l

firstOrderExpr :: Expr (Int -> FirstOrder)
firstOrderExpr =
  let app f a contentDepth = App (f contentDepth) (a contentDepth)
      lam str f contentDepth =
        let var varDepth = Var str (varDepth - (contentDepth + 1))
        in Lam str (f var (contentDepth + 1))
  in Expr{..}

computeFirstOrder :: (forall expr . Expr expr -> expr) -> FirstOrder
computeFirstOrder expr = expr firstOrderExpr 0

falseFirstOrder :: FirstOrder
falseFirstOrder = computeFirstOrder false
{-
normalFormExpr :: Expr (Int -> NormalForm)
normalFormExpr =
  let app f a contentDepth =
        case f contentDepth of
          NLam str normalF -> normalF (a contentDepth)
          Neutral n -> Neutral (NApp n (a contentDepth))
      lam str f contentDepth =
        let var varDepth = Neutral (NVar str (varDepth - (contentDepth + 1)))
        in NLam str (f var (contentDepth + 1))
  in Expr{..}
-}
