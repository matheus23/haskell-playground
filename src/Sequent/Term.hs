module Sequent.Term where

import Data.Functor.Foldable

data TermF a
  = Not a
  | a `Or` a
  | a `And` a
  | a `Implies` a
  deriving (Show, Eq, Read, Functor)

data TermOrVar a
  = Var String
  | Term (TermF a)
  deriving (Show, Eq, Read, Functor)

type Term = Fix TermOrVar

andTerm, orTerm, impliesTerm :: Term -> Term -> Term
andTerm a b = Fix (Term (a `And` b))
orTerm a b = Fix (Term (a `Or` b))
impliesTerm a b = Fix (Term (a `Implies` b))

var :: String -> Term
var = Fix . Var

exampleLeft :: Term
exampleLeft = (var "a" `impliesTerm` var "b") `andTerm` var "a"

example :: Term
example =
  exampleLeft `impliesTerm` var "b"
