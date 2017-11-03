module Sequent.Proof where

import Data.Functor.Foldable

import Sequent.Term

data ProofF a
  = Because [a] Sequent -- Add information about rules?
  deriving (Functor)

data Sequent = Sequent [Term] [Term]

type Proof = Fix ProofF

because :: [Proof] -> [Term] -> [Term] -> Proof
because reasons assumptions implications = Fix (Because reasons (Sequent assumptions implications))

exampleProof :: Proof
exampleProof =
  because
    []
    [var "c", impliesTerm (var "c") (var "a"), impliesTerm (var "c") (var "b")] [andTerm (var "a") (var "b")]
