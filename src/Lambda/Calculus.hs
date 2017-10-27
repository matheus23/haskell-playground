module Lambda.Calculus where

import qualified Data.Set as Set

type Name = String

data Lambda
  = Var Name
  | Abs Name Lambda
  | App Lambda Lambda
  deriving (Eq)

-- Correct Substitution

rename :: Name -> Name -> Lambda -> Lambda
rename toRename renamed (Var name)
  | name == toRename = Var renamed
  | otherwise        = Var name
rename toRename renamed (Abs arg body)
  | arg == toRename = Abs arg body -- the argument name shadows our name
  | otherwise       = Abs arg (rename toRename renamed body)
rename toRename renamed (App func arg) =
  App (rename toRename renamed func) (rename toRename renamed arg)

freeVariables :: Lambda -> Set.Set Name
freeVariables (Var name) = Set.singleton name
freeVariables (Abs arg body) = Set.delete arg (freeVariables body)
freeVariables (App func arg) = freeVariables func `Set.union` freeVariables arg

substitute :: String -> Lambda -> Lambda -> Lambda
substitute substName subst (Var name)
  | name == substName = subst
  | otherwise         = Var name
substitute substName subst (Abs arg body)
  | arg == substName = Abs arg body
  | arg `Set.member` freeVariables subst =
    let newArg = findFreeName arg (freeVariables subst)
     in Abs newArg (substitute substName subst (rename arg newArg body))
  | otherwise = Abs arg (substitute substName subst body)
substitute substName subst (App func arg) =
  App (substitute substName subst func) (substitute substName subst arg)

findFreeName :: String -> Set.Set String -> String
findFreeName basis takenNames
  | basisTick `Set.member` takenNames = findFreeName basisTick takenNames
  | otherwise = basisTick
  where basisTick = basis ++ "'"

-- Reduction & Normal Form

reduceLeftmostRedex :: Lambda -> Maybe Lambda
reduceLeftmostRedex (Var _) = Nothing
reduceLeftmostRedex (App (Abs argName body) arg) =
  Just (substitute argName arg body)
reduceLeftmostRedex (App func arg) =
  case reduceLeftmostRedex func of
    Just reducedFunc -> Just (App reducedFunc arg)
    Nothing -> App func <$> reduceLeftmostRedex arg
reduceLeftmostRedex (Abs arg body) =
  Abs arg <$> reduceLeftmostRedex body

normalForm :: Lambda -> Lambda
normalForm expression =
  maybe expression normalForm (reduceLeftmostRedex expression)
