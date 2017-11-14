module Logic.Propositional where

import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Set as Set
import Data.Set (Set)

import Data.Functor.Foldable
import Data.Functor.Classes
import Text.Show.Deriving
import Text.Read.Deriving
import Data.Eq.Deriving

data BiOperator
  = And
  | Or
  | Imply
  | Equiv
  deriving (Eq, Show, Read)

data ExprF a
  = Zero
  | One
  | Var String
  | Not a
  | Op BiOperator a a
  deriving (Functor)

$(deriveShow1 ''ExprF)
$(deriveRead1 ''ExprF)
$(deriveEq1 ''ExprF)

type Expr = Fix ExprF

type Model = Map.Map String Bool

-- Construct values

mkZero, mkOne :: Expr
mkZero = Fix Zero
mkOne = Fix One

mkVar :: String -> Expr
mkVar = Fix . Var

mkNot :: Expr -> Expr
mkNot = Fix . Not

mkAnd, mkOr, mkImply, mkEquiv :: Expr -> Expr -> Expr
mkAnd lhs rhs = Fix (Op And lhs rhs)
mkOr lhs rhs = Fix (Op Or lhs rhs)
mkImply lhs rhs = Fix (Op Imply lhs rhs)
mkEquiv lhs rhs = Fix (Op Equiv lhs rhs)

-- Compute Values

val :: [(String, Bool)] -> Expr -> Bool
val modelAsList = value (Map.fromList modelAsList)

valueAlg :: Model -> ExprF Bool -> Bool
valueAlg _ Zero = False
valueAlg _ One = True
valueAlg model (Var var) = model ! var
valueAlg model (Not expr) = not expr
valueAlg model (Op operator left right) = interpretOperator operator left right

value :: Model -> Expr -> Bool
value model = fold (valueAlg model)

interpretOperator :: BiOperator -> Bool -> Bool -> Bool
interpretOperator And = (&&)
interpretOperator Or = (||)
interpretOperator Imply = \left right -> not left || right
interpretOperator Equiv = (==)

-- Eqivalence and Tautology

interpretationEquivalence :: Expr -> Expr -> Bool
interpretationEquivalence exprA exprB = isTautology (exprA `mkEquiv` exprB)

isTautology :: Expr -> Bool
isTautology expr =
  isTautologyWith (freeVariables expr) expr

isTautologyWith :: Set String -> Expr -> Bool
isTautologyWith vars expr =
    all (`val` expr) (allModels (Set.toList vars))

allModels :: [String] -> [[(String, Bool)]]
allModels [] = [[]]
allModels (var:vars) = do
    truth <- [(var, True), (var, False)]
    model <- allModels vars
    return (truth : model)

freeVariablesAlg :: ExprF (Set String) -> Set String
freeVariablesAlg (Var v) = Set.singleton v
freeVariablesAlg (Op _ freeLeft freeRight) = freeLeft `Set.union` freeRight
freeVariablesAlg (Not freeVars) = freeVars
freeVariablesAlg _ = Set.empty

freeVariables :: Expr -> Set String
freeVariables = fold freeVariablesAlg

-- Rewrite Rules
{- I don't think I'll need this
type RewriteRule = Expr -> Maybe Expr

deMorgan :: RewriteRule
deMorgan (Not (Op And lhs rhs)) = Just (Op Or (Not lhs) (Not rhs))
deMorgan (Not (Op Or lhs rhs)) = Just (Op And (Not lhs) (Not rhs))
deMorgan _ = Nothing

orIdentity :: RewriteRule
orIdentity (Op Or lhs Zero) = Just lhs
orIdentity (Op Or Zero rhs) = Just rhs
orIdentity _ = Nothing

andIdentity :: RewriteRule
andIdentity (Op And lhs One) = Just lhs
andIdentity (Op And One rhs) = Just rhs
andIdentity _ = Nothing

orIdempotent :: RewriteRule
orIdempotent (Op Or lhs One) = Just One
orIdempotent (Op Or One rhs) = Just One
orIdempotent _ = Nothing

andIdempotent :: RewriteRule
andIdempotent (Op And lhs Zero) = Just Zero
andIdempotent (Op And Zero rhs) = Just Zero
andIdempotent _ = Nothing

orEqIdentity :: RewriteRule
orEqIdentity (Op Or lhs rhs) | lhs == rhs = Just lhs
orEqIdentity _ = Nothing

andEqIdentity :: RewriteRule
andEqIdentity (Op And lhs rhs) | lhs == rhs = Just lhs
andEqIdentity _ = Nothing

-}
