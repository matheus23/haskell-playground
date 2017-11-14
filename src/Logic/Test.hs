{-# LANGUAGE FlexibleInstances #-}
module Logic.Test where

import Logic.Propositional
import Logic.PrettyPrinter
import Logic.Parser
import qualified Logic.AndOrNotBase as Base

import Test.QuickCheck
import Data.Functor.Foldable
import Data.Function
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

instance Arbitrary BiOperator where
  arbitrary = elements [And, Or, Imply, Equiv]

instance Arbitrary Expr where
  arbitrary = oneof
    [ pure mkZero
    , pure mkOne
    , mkVar . singleton <$> elements "abcdefg"
    , mkNot <$> arbitrary
    , Fix <$> (Op <$> arbitrary <*> arbitrary <*> arbitrary)
    ]
    where singleton x = [x]

  shrink (Fix (Not expr)) = [expr]
  shrink (Fix (Op _ left right)) = [left, right]
  shrink other = []


instance Arbitrary Base.AndOrNot where
  arbitrary = sized $ \n ->
      if n <= 0 then
        Fix <$> (Base.Var <$> (singleton <$> elements "abcdefg"))
      else
        oneof
          [ Fix <$> (Base.Var <$> (singleton <$> elements "abcdefg"))
          , Fix <$> (Base.And <$> resize (n `div` 2) arbitrary)
          , Fix <$> (Base.Or <$> resize (n `div` 2) arbitrary)
          ]
    where singleton x = [x]

  shrink (Fix (Base.And exprs)) = exprs
  shrink (Fix (Base.Or exprs)) = exprs
  shrink _ = []

instance Arbitrary Base.NotNormalForm where
  arbitrary = sized $ \n ->
      if n <= 0 then
        Fix <$> (Base.Lit <$> arbitrary <*> (singleton <$> elements "abcdefg"))
      else
        oneof
          [ Fix <$> (Base.Lit <$> arbitrary <*> (singleton <$> elements "abcdefg"))
          , Fix <$> (Base.NNFAnd <$> resize (n `div` 2) arbitrary)
          , Fix <$> (Base.NNFOr <$> resize (n `div` 2) arbitrary)
          ]
    where singleton x = [x]

  shrink (Fix (Base.NNFAnd exprs)) = exprs
  shrink (Fix (Base.NNFOr exprs)) = exprs
  shrink _ = []

prop_readShowEquality :: Expr -> Property
prop_readShowEquality expression =
  expression === read (show expression)

prop_parsePrettyPrintEquality :: Expr -> Property
prop_parsePrettyPrintEquality expression =
  expression === parseExpr (prettyPrintExpr expression)

prop_baseConversionInterpretationEquality :: Expr -> Property
prop_baseConversionInterpretationEquality expression =
  forallModels (freeVariables expression) $ \model ->
    value model expression === Base.value model (Base.fromHumane expression)

prop_negateNotNormalFormNegates :: Base.NotNormalForm -> Property
prop_negateNotNormalFormNegates nnf =
  forallModels (Base.freeVariablesNNF nnf) $ \model ->
    Base.valueNNF model nnf === not (Base.valueNNF model (Base.negateNNF nnf))

prop_computeNNFPreservesInterpretation :: Base.AndOrNot -> Property
prop_computeNNFPreservesInterpretation expression =
  forallModels (Base.freeVariables expression) $ \model ->
    Base.value model expression === Base.valueNNF model (Base.computeNNF expression)

forallModels :: Set String -> (Map String Bool -> Property) -> Property
forallModels freeVars prop =
  conjoin ((prop . Map.fromList) <$> allModels (Set.toList freeVars))

return []
runTests = $quickCheckAll
