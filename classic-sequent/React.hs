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

type React a = Reactive Input (Maybe a)

toReactive :: (m -> React m) -> m -> Reactive Input m
toReactive react model = fromMaybe model <$> react model

atopReact :: React a -> React a -> React a
atopReact (Reactive reactAtop visualAtop) (Reactive reactBelow visualBelow) =
    Reactive react (visualAtop `atop` visualBelow)
  where
    react event = getFirst (First (reactAtop event) <> First (reactBelow event))

besidesTo :: V2 Double -> React a -> React a -> React a
besidesTo dir =
    Reactive.besidesTo dir combine
  where
    combine a b = getFirst (First a <> First b)

besidesRight, besidesDown :: React a -> React a -> React a
besidesRight = besidesTo right
besidesDown = besidesTo down

onEventPre :: (Input -> Maybe a) -> React a -> React a
onEventPre onEvent =
    Reactive.onEvent handleEvent
  where
    handleEvent event maybeModel = onEvent event

onEventPost :: (Input -> Maybe a) -> React a -> React a
onEventPost onEvent =
    Reactive.onEvent handleEvent
  where
    handleEvent event Nothing = onEvent event
    handleEvent event justModel = justModel

filterInside :: HasBorder b => b -> (Input -> Maybe a) -> Input -> Maybe a
filterInside bordered func event
  | Reactive.eventInside bordered event = func event
  | otherwise = Nothing
