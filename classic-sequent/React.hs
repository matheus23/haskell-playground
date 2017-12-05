module React where

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
import FormUtils
import Data.Maybe
import Data.Monoid

type React a = Reactive Input (a, Bool)

toReactive :: React a -> Reactive Input a
toReactive = fmap fst

makeReact :: Reactive Input a -> React a
makeReact reactive =
  Reactive.onEvent attachInside reactive
  where
    attachInside event@(MouseInput (MousePress _ _)) model
      | Reactive.eventInside reactive event = (model, True)
      | otherwise = (model, False)
    attachInside event model = (model, False)

besidesTo :: V2 Double -> React (a -> b) -> React a -> React b
besidesTo dir = Reactive.besidesTo dir combine
  where combine (f, refConsumed) (a, attConsumed) = (f a, refConsumed || attConsumed)

besidesRight, besidesDown :: React (a -> b) -> React a -> React b
besidesRight = besidesTo right
besidesDown = besidesTo down
