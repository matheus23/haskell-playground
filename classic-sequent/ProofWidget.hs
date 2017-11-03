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

import Sequent
import TermWidget

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
