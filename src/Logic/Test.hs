module Logic.Test where

import Logic.Propositional
import Logic.PrettyPrinter
import Logic.Parser

import Test.QuickCheck

instance Arbitrary BiOperator where
  arbitrary = elements [And, Or, Imply, Equiv]

instance Arbitrary Expr where
  arbitrary = oneof
    [ pure Zero
    , pure One
    , Var . singleton <$> elements "abcdefg"
    , Not <$> arbitrary
    , Op <$> arbitrary <*> arbitrary <*> arbitrary
    ]
    where singleton x = [x]

  shrink (Not expr) = [expr]
  shrink (Op _ left right) = [left, right]
  shrink other = [other]

prop_parseEquality :: Expr -> Property
prop_parseEquality expression =
  expression === parseExpr (show expression)
