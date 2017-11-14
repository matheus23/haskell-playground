module Lambda.Test where

import Lambda.Untyped
import Lambda.PrettyPrint
import Lambda.Parser
import Lambda.Expressions

import Test.QuickCheck
import Data.Function
import qualified Data.Set as Set

randomName :: Gen String
randomName = do
  size <- choose (1, 5)
  resize size (listOf1 randomAlphaNum)

randomAlphaNum :: Gen Char
randomAlphaNum = elements identifierChars

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

{-
this one is ALSO wrong...

counterexample: λb. a
                λb. a /= λb. b

--prop_rename :: Lambda -> Property
--prop_rename expression =
  not (Set.member "b" (freeVariables expression)) ==>
  expression === (expression & rename "a" "b" & rename "b" "a")


oh god. even more wrong stuff. I can't do this, since I might have
free variables that are shadowed when renamed.

counterexample: (λb. a) b /= (λb. b) b

--prop_rename :: Lambda -> Property
--prop_rename expression =
  let free = freeVariables expression
      (free1:free2:_) = Set.toList free
   in Set.size free >= 2 && not (Set.member "placeholder" free) ==>
      expression === (expression & switchVars free1 free2 & switchVars free2 free1)
  where
    switchVars var1 var2 =
      rename "placeholder" var2
      . rename var2 var1
      . rename var1 "placeholder"

I find it fascinating, how many statements you believe to be true, but aren't!
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

-- Any higher linewidths would slow down testing a lot (!)
prop_parserPrettyPrinterIdentity :: Lambda -> Property
prop_parserPrettyPrinterIdentity expression =
  expression === parseLambda (render 20 (pretty expression))

-- Actual Expression tests

interpretationEquality :: (LambdaValue a, LambdaValue r, Show r, Eq r) => (a -> r) -> Lambda -> a -> Property
interpretationEquality function functionAsLambda value =
  function value === interpret (functionAsLambda `App` toLambda value)

interpretationEquality2 :: (LambdaValue a2, LambdaValue a1, LambdaValue r, Show r, Eq r) => (a1 -> a2 -> r) -> Lambda -> a1 -> a2 -> Property
interpretationEquality2 function functionAsLambda value1 value2 =
  function value1 value2 === interpret (functionAsLambda `App` toLambda value1 `App` toLambda value2)

prop_interpretBinaryBool :: Bool -> Bool -> Property
prop_interpretBinaryBool lhs rhs =
  conjoin
    (map (\(int, op) -> interpretationEquality2 int op lhs rhs)
      [ ((==), boolEq)
      , ((||), boolOr)
      , ((&&), boolAnd)
      ])

prop_interpretNotEq :: Bool -> Property
prop_interpretNotEq =
  interpretationEquality not boolNot

-- booleans are encoded as "if"s in lambda calculus
prop_interpretBoolIf :: Bool -> Bool -> Bool -> Property
prop_interpretBoolIf prop true false =
  (if prop then true else false) === interpret (toLambda prop `App` toLambda true `App` toLambda false)

prop_interpretPlus :: Positive Int -> Positive Int -> Property
prop_interpretPlus (Positive a) (Positive b) = interpretationEquality2 (+) natPlus a b

return []
runTests = $quickCheckAll
