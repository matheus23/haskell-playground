module Logic.AndOrNotBase where

import qualified Logic.Propositional as Prop
import Data.Functor.Foldable
import qualified Data.Map as Map
import Data.Map ((!), Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Show.Deriving
import Text.Read.Deriving
import Data.Eq.Deriving

data AndOrNotF a
  = Var String
  | Not a
  | Or [a]
  | And [a]
  deriving (Functor)

data NotNormalFormF a
  = Lit Bool String
  | NNFOr [a]
  | NNFAnd [a]
  deriving (Functor)

$(deriveShow1 ''AndOrNotF)
$(deriveRead1 ''AndOrNotF)
$(deriveEq1 ''AndOrNotF)

$(deriveShow1 ''NotNormalFormF)
$(deriveRead1 ''NotNormalFormF)
$(deriveEq1 ''NotNormalFormF)

type AndOrNot = Fix AndOrNotF
type NotNormalForm = Fix NotNormalFormF

mkVar :: String -> AndOrNot
mkVar = Fix . Var

mkNot :: AndOrNot -> AndOrNot
mkNot = Fix . Not

mkOr, mkAnd :: [AndOrNot] -> AndOrNot
mkOr = Fix . Or
mkAnd = Fix . And

mkLit :: Bool -> String -> NotNormalForm
mkLit negated = Fix . Lit negated

mkNNFOr, mkNNFAnd :: [NotNormalForm] -> NotNormalForm
mkNNFOr = Fix . NNFOr
mkNNFAnd = Fix . NNFAnd

fromHumaneAlg :: Prop.ExprF AndOrNot -> AndOrNot
fromHumaneAlg Prop.Zero = mkOr []
fromHumaneAlg Prop.One = mkAnd []
fromHumaneAlg (Prop.Var str) = mkVar str
fromHumaneAlg (Prop.Not expr) = mkNot expr
fromHumaneAlg (Prop.Op Prop.And lhs rhs) = mkAnd [lhs, rhs]
fromHumaneAlg (Prop.Op Prop.Or lhs rhs) = mkOr [lhs, rhs]
fromHumaneAlg (Prop.Op Prop.Imply lhs rhs) = mkOr [mkNot lhs, rhs]
fromHumaneAlg (Prop.Op Prop.Equiv lhs rhs) =
  mkOr [mkAnd [lhs, rhs], mkAnd [mkNot lhs, mkNot rhs]]

fromHumane :: Prop.Expr -> AndOrNot
fromHumane = fold fromHumaneAlg

valueAlg :: Map String Bool -> AndOrNotF Bool -> Bool
valueAlg model (Var str) = model ! str
valueAlg model (Not val) = not val
valueAlg model (Or exprs) = or exprs
valueAlg model (And exprs) = and exprs

value :: Map String Bool -> AndOrNot -> Bool
value model = fold (valueAlg model)

freeVariablesAlg :: AndOrNotF (Set String) -> Set String
freeVariablesAlg (Var str) = Set.singleton str
freeVariablesAlg (Not free) = free
freeVariablesAlg (Or exprs) = foldl Set.union Set.empty exprs
freeVariablesAlg (And exprs) = foldl Set.union Set.empty exprs

freeVariables :: AndOrNot -> Set String
freeVariables = fold freeVariablesAlg

negateNNFHom :: NotNormalFormF NotNormalForm -> NotNormalFormF NotNormalForm
negateNNFHom (Lit negated name) = Lit (not negated) name
negateNNFHom (NNFOr exprs) = NNFAnd exprs
negateNNFHom (NNFAnd exprs) = NNFOr exprs

negateNNF :: NotNormalForm -> NotNormalForm
negateNNF = fold (Fix . negateNNFHom)

valueAlgNNF :: Map String Bool -> NotNormalFormF Bool -> Bool
valueAlgNNF model (Lit negated name) = (if negated then not else id) (model ! name)
valueAlgNNF _ (NNFOr bools) = or bools
valueAlgNNF _ (NNFAnd bools) = and bools

valueNNF :: Map String Bool -> NotNormalForm -> Bool
valueNNF model = fold (valueAlgNNF model)

freeVariablesAlgNNF :: NotNormalFormF (Set String) -> Set String
freeVariablesAlgNNF (Lit _ name) = Set.singleton name
freeVariablesAlgNNF (NNFOr freeVars) = foldl Set.union Set.empty freeVars
freeVariablesAlgNNF (NNFAnd freeVars) = foldl Set.union Set.empty freeVars

freeVariablesNNF :: NotNormalForm -> Set String
freeVariablesNNF = fold freeVariablesAlgNNF

computeNNFAlg :: AndOrNotF NotNormalForm -> NotNormalForm
computeNNFAlg (Not nnf) = negateNNF nnf -- the only interesting case
computeNNFAlg (Var name) = mkLit False name
computeNNFAlg (Or exprs) = mkNNFOr exprs
computeNNFAlg (And exprs) = mkNNFAnd exprs

computeNNF :: AndOrNot -> NotNormalForm
computeNNF = fold computeNNFAlg
