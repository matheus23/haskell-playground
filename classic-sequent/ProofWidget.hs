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
import qualified Event
import RunReactive (runReactive)
import Linear
import Data.Functor.Foldable
import Utils (isInside)
import FormUtils

import Widgets.Activatable (ActiveOr(..))
import Widgets.Activatable as Activatable

import Sequent
import TermWidget

newtype FocusableProofF a =
  Focusable (ActiveOr (ProofF a) (ProofF a))

instance Functor FocusableProofF where
  fmap f (Focusable (Active proof)) = Focusable (Active (fmap f proof))
  fmap f (Focusable (Inactive proof)) = Focusable (Inactive (fmap f proof))

type FocusableProof = Fix FocusableProofF

nothingFocused :: Proof -> FocusableProof
nothingFocused = cata (Fix . Focusable . Inactive)

viewFocusableProof :: TextStyle -> FocusableProof -> Reactive Input FocusableProof
viewFocusableProof style = cata (fmap Fix . viewFocusableProofF style)

viewFocusableProofF :: TextStyle -> FocusableProofF (Reactive Input a) -> Reactive Input (FocusableProofF a)
viewFocusableProofF style (Focusable activatableProof) =
  Focusable <$>
    case activatableProof of
      (Active proofF) ->
        let activeReactive = Reactive.onVisual (addBorder lightBlue) (viewProofF style proofF)
            maybeMakeInactive (MouseInput (MousePress pos MBLeft))
              | not (isInside activeReactive pos) = Inactive
            maybeMakeInactive _ = Active
         in Reactive.onEvent maybeMakeInactive activeReactive

      (Inactive proofF) ->
        let inactiveReactive = viewProofF style proofF
            maybeMakeActive (MouseInput (MousePress pos MBLeft))
              | isInside inactiveReactive pos = Active
            maybeMakeActive _ = Inactive
         in Reactive.onEvent maybeMakeActive inactiveReactive

viewProof :: TextStyle -> Proof -> Reactive Input Proof
viewProof style = cata (fmap Fix . viewProofF style)

viewProofF :: TextStyle -> ProofF (Reactive Input a) -> Reactive Input (ProofF a)
viewProofF style (Because [] sequent) = Because [] <$> viewTurnstile style sequent
viewProofF style (Because reasons sequent) =
    (Because <$> centeredHV reasonsReactive)
    `Reactive.besidesDown`
    Reactive.attachFormTo up line (centeredHV turnstileReactive)
  where
    reasonsReactive = Reactive.separatedBy right (text style "    ") reasons
    turnstileReactive = viewTurnstile style sequent
    maxWidth = max (graphicWidth (visual reasonsReactive)) (graphicWidth (visual turnstileReactive))
    halfWidth = maxWidth / 2
    line = outlined (solid black) (noBorder (openPath (pathPoint (-halfWidth, 0) `lineConnect` pathPoint (halfWidth, 0))))

viewTurnstile :: TextStyle -> Sequent -> Reactive Input Sequent
viewTurnstile style (Sequent assumptions implications) =
    (Sequent <$> Reactive.separatedBy right (text style ", ") (map (viewTerm style) assumptions))
    `Reactive.besidesRight` Reactive.attachFormTo left (text style " ⊢ ")
      (Reactive.separatedBy right (text style ", ") (map (viewTerm style) implications))
