{-# LANGUAGE FlexibleInstances #-}
module SExpr.Test where

import Test.QuickCheck
import SExpr.Types
import SExpr.PrettyPrint
import SExpr.Parser

import Data.Functor.Foldable

randomName :: Gen String
randomName = do
  size <- choose (1, 5)
  resize size (listOf1 randomAlphaNum)

randomAlphaNum :: Gen Char
randomAlphaNum = elements identifierChars

instance Arbitrary SExpr where
  arbitrary = sized $ \n ->
    if n <= 0 then
      Fix . Symbol <$> randomName
    else
      oneof
        [ Fix . Symbol <$> resize (n - 1) randomName
        , do
          (len :: Int) <- choose (0, n)
          Fix . List <$> vectorOf len (resize (n `div` len) arbitrary)
        ]

  shrink (Fix (Symbol str)) = Fix . Symbol <$> shrink str
  shrink (Fix (List elems)) = elems

prop_parserPrettyPrinterIdentity :: SExpr -> Property
prop_parserPrettyPrinterIdentity sexpr =
  sexpr === parseSExpr (render 42 (prettyPrintSExpr sexpr))

return []
runTests = $quickCheckAll
