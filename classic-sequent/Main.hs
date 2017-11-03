{-# LANGUAGE DeriveFunctor #-}
module Main where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input

import qualified Reactive
import Reactive (Reactive(..))
import qualified Event
import RunReactive (runReactive)
import Data.Monoid (First(..))
import Utils (orElse, isInside, orTry, rightAngle)
import Linear
import FormUtils

data TermF a
  = Not a
  | a `Or` a
  | a `And` a
  | a `Implies` a
  deriving (Show, Eq, Read, Functor)

newtype Fix f = Fix { unfix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata algebra fix = algebra (cata algebra <$> unfix fix)

data TermOrVar a
  = Var String
  | Term (TermF a)
  deriving (Show, Eq, Read, Functor)

type Term = Fix TermOrVar

data ProofF a
  = Because [a] Sequent -- Add information about rules?
  deriving (Functor)

data Sequent = Sequent [Term] [Term]

type Proof = Fix ProofF

andTerm, orTerm, impliesTerm :: Term -> Term -> Term
andTerm a b = Fix (Term (a `And` b))
orTerm a b = Fix (Term (a `Or` b))
impliesTerm a b = Fix (Term (a `Implies` b))

var :: String -> Term
var = Fix . Var

because :: [Proof] -> [Term] -> [Term] -> Proof
because reasons assumptions implications = Fix (Because reasons (Sequent assumptions implications))

exampleLeft :: Term
exampleLeft = (var "a" `impliesTerm` var "b") `andTerm` var "a"

example :: Term
example =
  exampleLeft `impliesTerm` var "b"

exampleProof :: Proof
exampleProof =
  because
    []
    [var "c", impliesTerm (var "c") (var "a"), impliesTerm (var "c") (var "b")] [andTerm (var "a") (var "b")]

mtext :: String -> Form
mtext = text defaultTextStyle { fontSize = 20 }

main :: IO ()
main = runReactive (move (V2 400 500) . alignHV (0.5, 1) . viewProof) exampleProof



viewProof :: Proof -> Reactive Input Proof
viewProof = cata (fmap Fix . viewProofF)

viewProofF :: ProofF (Reactive Input a) -> Reactive Input (ProofF a)
viewProofF (Because [] sequent) = Because [] <$> viewTurnstile sequent
viewProofF (Because reasons sequent) =
    (Because <$> centeredHV reasonsReactive)
    `Reactive.besidesDown`
    Reactive.attachFormTo up line (centeredHV turnstileReactive)
  where
    reasonsReactive = Reactive.separatedBy right (mtext "    ") reasons
    turnstileReactive = viewTurnstile sequent
    maxWidth = max (graphicWidth (visual reasonsReactive)) (graphicWidth (visual turnstileReactive))
    halfWidth = maxWidth / 2
    line = outlined (solid black) (noBorder (openPath (pathPoint (-halfWidth, 0) `lineConnect` pathPoint (halfWidth, 0))))

viewTurnstile :: Sequent -> Reactive Input Sequent
viewTurnstile (Sequent assumptions implications) =
    (Sequent <$> Reactive.separatedBy right (mtext ", ") (map viewTerm assumptions))
    `Reactive.besidesRight` Reactive.attachFormTo left (mtext " ⊢ ")
      (Reactive.separatedBy right (mtext ", ") (map viewTerm implications))


viewString :: String -> Reactive Input String
viewString = Reactive.fromModel mtext

viewTerm :: Term -> Reactive Input Term
viewTerm term = viewTermPrec term 0

viewTermPrec :: Term -> Int -> Reactive Input Term
viewTermPrec = cata (\tov p -> Fix <$> viewTermOrVarPrec tov p)

viewTermOrVarPrec :: TermOrVar (Int -> Reactive Input a) -> Int -> Reactive Input (TermOrVar a)
viewTermOrVarPrec (Var str) prec = Var <$> viewString str
viewTermOrVarPrec (Term termf) prec = Term <$> viewTermFPrec termf prec

viewTermFPrec :: TermF (Int -> Reactive Input a) -> Int -> Reactive Input (TermF a)
viewTermFPrec (Not a) prec = Reactive.attachFormTo left (mtext "¬") (Not <$> a 5)
viewTermFPrec (a `Or` b) prec =
  parenWhen (prec > 2) $
  Reactive.attachFormTo right (mtext " ∨ ") (Or <$> a 3)
  `Reactive.besidesRight` b 2
viewTermFPrec (a `And` b) prec =
  parenWhen (prec > 3) $
  Reactive.attachFormTo right (mtext " ∧ ") (And <$> a 4)
  `Reactive.besidesRight` b 3
viewTermFPrec (a `Implies` b) prec =
  parenWhen (prec > 2) $
  Reactive.attachFormTo right (mtext " → ") (Implies <$> a 3)
  `Reactive.besidesRight` b 2

parenWhen :: Transformable e => Bool -> Reactive e a -> Reactive e a
parenWhen True = addParens
parenWhen False = id

addParens :: Transformable e => Reactive e a -> Reactive e a
addParens =
    Reactive.attachFormTo left leftParens . Reactive.attachFormTo right rightParens
  where
    leftParens = mtext "("
    rightParens = mtext ")"
