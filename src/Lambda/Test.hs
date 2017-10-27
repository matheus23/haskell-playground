{-# LANGUAGE TemplateHaskell #-}
module Lambda.Test where

import Lambda.Calculus
import Lambda.PrettyPrint
import Lambda.Parser

import Test.QuickCheck
import Data.Function
import qualified Data.Set as Set

randomName :: Gen String
randomName = do
  size <- choose (1, 5)
  resize size (listOf1 randomAlphaNum)

randomAlphaNum :: Gen Char
randomAlphaNum = elements "abcdefghijklmnopqrstuvwxyz0123456789_"

randomAlpha :: Gen String
randomAlpha = singleton <$> elements "abcdefghijklmnopqrstuvwxyz"
  where singleton x = [x]

instance Arbitrary Lambda where
  arbitrary =
    sized $ \n ->
      if n == 0
        then Var <$> randomName
        else
          oneof
            [ Abs <$> randomName <*> scale (subtract 1) arbitrary
            , do
                m <- choose (0, n)
                App <$> resize m arbitrary <*> resize (n - m) arbitrary
            ]

  shrink (Var name) = []
  shrink (App func arg) = [func, arg]
  shrink (Abs name body)
    | name `Set.member` freeVariables body = []
    | otherwise                            = [body]

{- bullshit property hahaaa
--prop_rename :: Lambda -> Property
--prop_rename expression =
--  expression === (expression & rename "a" "b" & rename "b" "a")
-}

prop_renameNotInFreeVariables :: Lambda -> Property
prop_renameNotInFreeVariables expression =
    forAll distinctNames $ \(nameA, nameB) ->
      nameA `Set.notMember` freeVariables (rename nameA nameB expression)
  where
    distinctNames = do
      nameA <- randomName
      nameB <- randomName `suchThat` (/= nameA)
      return (nameA, nameB)

prop_substitutionRemovesFreeVariable :: Lambda -> Lambda -> Property
prop_substitutionRemovesFreeVariable expression substitution =
  let freeVarList = Set.toList (freeVariables expression `Set.difference` freeVariables substitution)
   in not (null freeVarList) ==>
      forAll (elements freeVarList) $ \freeVar ->
        freeVar `Set.notMember` freeVariables (substitute freeVar substitution expression)

prop_parserPrinterIdentity :: Lambda -> Property
prop_parserPrinterIdentity expression =
  expression === parseLambda (show expression)

return []
runTests = $quickCheckAll
