module TellingTextField where

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

import qualified Widgets.TextField as TextField
import qualified Widgets.Activatable as Activatable

type Model = TextField.Model

view :: TextStyle -> String -> Model -> Reactive Input (Model, Bool)
view style placeholder model =
    Reactive.onEvent handleEvent textFieldReactive
  where
    textFieldReactive = TextField.view style placeholder model
    handleEvent ev model =
      let newModel = Reactive.react textFieldReactive ev
       in (newModel, isActive newModel)

    isActive (Activatable.Active _) = True
    isActive _ = False

    isMousePress (MouseInput (MousePress _ _)) = True
    isMousePress _ = False
