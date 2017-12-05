module ProofWidget where

import Graphics.Declarative.Classes
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input

import qualified Reactive
import Reactive (Reactive(..))
import qualified React
import React (React)
import qualified Event
import RunReactive (runReactive)
import Linear
import Data.Functor.Foldable
import Utils (isInside)
import FormUtils
import Control.Arrow (first)

import Widgets.Activatable (ActiveOr(..))
import Widgets.Activatable as Activatable

import Sequent
import TermWidget

data FocusableF f a
  = FocusableF Bool (f a)
  deriving Functor

type FocusableProof = Fix (FocusableF ProofF)

nothingFocused :: Proof -> FocusableProof
nothingFocused = cata (Fix . FocusableF False)

viewFocusableProof :: TextStyle -> FocusableProof -> React FocusableProof
viewFocusableProof style = cata (fmap (first Fix) . viewFocusableF (viewProofF style))

viewFocusableF :: (f (React a) -> React (f a)) -> FocusableF f (React a) -> React (FocusableF f a)
viewFocusableF viewInner (FocusableF focused inner) =
    Reactive.onEvent handleEvent reactive
  where
    reactive = Reactive.onVisual (if focused then addBorder lightBlue else id) (viewInner inner)
    handleEvent event@(MouseInput (MousePress _ _)) (model, False)
      | Reactive.eventInside reactive event = (FocusableF True model, True)
      | otherwise                           = (FocusableF False model, False)
    handleEvent event (model, consumed)     = (FocusableF focused model, consumed)

viewProofF :: TextStyle -> ProofF (React a) -> React (ProofF a)
viewProofF style (Because [] sequent) = first (Because []) <$> viewTurnstile style sequent
viewProofF style (Because reasons sequent) =
    (combine <$> centeredHV reasonsReactive)
    `React.besidesDown`
    Reactive.attachFormTo up line (centeredHV turnstileReactive)
  where
    combine as = (Because (map fst as), any snd as)
    reasonsReactive = Reactive.separatedBy right (text style "    ") reasons
    turnstileReactive = viewTurnstile style sequent
    maxWidth = max (graphicWidth (visual reasonsReactive)) (graphicWidth (visual turnstileReactive))
    halfWidth = maxWidth / 2
    line = outlined (solid black) (noBorder (openPath (pathPoint (-halfWidth, 0) `lineConnect` pathPoint (halfWidth, 0))))

viewTurnstile :: TextStyle -> Sequent -> React Sequent
viewTurnstile style (Sequent assumptions implications) =
    React.makeReact $
      (Sequent <$> Reactive.separatedBy right (text style ", ") (map (viewTerm style) assumptions))
      `Reactive.besidesRight` Reactive.attachFormTo left (text style " ⊢ ")
        (Reactive.separatedBy right (text style ", ") (map (viewTerm style) implications))
