module Logic.Propositional where

import qualified Data.Map as Map
import Data.Map ((!))

import Text.PrettyPrint

data BiOperator
  = And
  | Or
  | Imply
  | Equiv
  deriving (Eq)

data Expr
  = Zero
  | One
  | Var String
  | Not Expr
  | Op BiOperator Expr Expr
  deriving (Eq)

type Model = Map.Map String Bool

-- Compute Values

val :: [(String, Bool)] -> Expr -> Bool
val modelAsList = value (Map.fromList modelAsList)

value :: Model -> Expr -> Bool
value _ Zero = False
value _ One = True
value model (Var var) = model ! var
value model (Not expr) = not (value model expr)
value model (Op operator left right) = interpretOperator operator (value model left) (value model right)

interpretOperator :: BiOperator -> Bool -> Bool -> Bool
interpretOperator And = (&&)
interpretOperator Or = (||)
interpretOperator Imply = \left right -> not left || right
interpretOperator Equiv = (==)

-- Eqivalence and Tautology

interpretationEquivalence :: Expr -> Expr -> Bool
interpretationEquivalence exprA exprB = isTautology (Op Equiv exprA exprB)

isTautology :: Expr -> Bool
isTautology expr =
  isTautologyWith (freeVariables expr) expr

isTautologyWith :: [String] -> Expr -> Bool
isTautologyWith vars expr =
    all (`val` expr) (allModels vars)

allModels :: [String] -> [[(String, Bool)]]
allModels [] = [[]]
allModels (var:vars) = do
    truth <- [(var, True), (var, False)]
    model <- allModels vars
    return (truth : model)

freeVariables :: Expr -> [String]
freeVariables (Var v) = [v]
freeVariables (Op _ left right) = freeVariables left ++ freeVariables right
freeVariables (Not expr) = freeVariables expr
freeVariables _ = []

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
