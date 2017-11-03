module TermWidget where

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
import Linear

import Sequent
import Data.Functor.Foldable

viewString :: TextStyle -> String -> Reactive Input String
viewString style = Reactive.fromModel (text style)

viewTerm :: TextStyle -> Term -> Reactive Input Term
viewTerm style term = viewTermPrec style term 0

viewTermPrec :: TextStyle -> Term -> Int -> Reactive Input Term
viewTermPrec style = cata (\tov p -> Fix <$> viewTermOrVarPrec style tov p)

viewTermOrVarPrec :: TextStyle -> TermOrVar (Int -> Reactive Input a) -> Int -> Reactive Input (TermOrVar a)
viewTermOrVarPrec style (Var str) prec = Var <$> viewString style str
viewTermOrVarPrec style (Term termf) prec = Term <$> viewTermFPrec style termf prec

viewTermFPrec :: TextStyle -> TermF (Int -> Reactive Input a) -> Int -> Reactive Input (TermF a)
viewTermFPrec style (Not a) prec = Reactive.attachFormTo left (text style "¬") (Not <$> a 5)
viewTermFPrec style (a `Or` b) prec =
  parenWhen style (prec > 2) $
  Reactive.attachFormTo right (text style " ∨ ") (Or <$> a 3)
  `Reactive.besidesRight` b 2
viewTermFPrec style (a `And` b) prec =
  parenWhen style (prec > 3) $
  Reactive.attachFormTo right (text style " ∧ ") (And <$> a 4)
  `Reactive.besidesRight` b 3
viewTermFPrec style (a `Implies` b) prec =
  parenWhen style (prec > 2) $
  Reactive.attachFormTo right (text style " → ") (Implies <$> a 3)
  `Reactive.besidesRight` b 2

parenWhen :: Transformable e => TextStyle -> Bool -> Reactive e a -> Reactive e a
parenWhen style True = addParens style
parenWhen style False = id

addParens :: Transformable e => TextStyle -> Reactive e a -> Reactive e a
addParens style =
    Reactive.attachFormTo left (text style "(")
    . Reactive.attachFormTo right (text style ")")
