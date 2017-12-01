{-# LANGUAGE RecordWildCards #-}
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

import qualified TellingTextField
import qualified Widgets.TextField as TextField

data Model
  = TwoTextFields
  { focused :: Bool
  , leftTF :: TellingTextField.Model
  , rightTF :: TellingTextField.Model
  }

initialModel = TwoTextFields False TextField.emptyInactive TextField.emptyInactive

main :: IO ()
main = runReactive view initialModel

view :: Model -> Reactive Input Model
view = fmap fst . viewTelling

viewTelling :: Model -> Reactive Input (Model, Bool)
viewTelling TwoTextFields{..} =
    (if focused then Reactive.onVisual (addBorder lightBlue) else id)
      (Reactive.onEvent handleEvent reactive)
  where
    reactive =
      (combine <$> leftReactive)
      `Reactive.besidesRight`
      rightReactive

    leftReactive =
      Reactive.attachFormTo right (text defaultTextStyle " ++ ")
        (TellingTextField.view defaultTextStyle "left" leftTF)

    rightReactive = TellingTextField.view defaultTextStyle "right" rightTF

    combine (modelLeft, consumedLeft) (modelRight, consumedRight) =
      (TwoTextFields focused modelLeft modelRight, consumedLeft || consumedRight)

    handleEvent event (model, True) = (model { focused = False }, True)
    handleEvent ev@(MouseInput (MousePress _ _)) (model, False)
      | Reactive.eventInside reactive ev = (model { focused = True }, True)
    handleEvent other (model, False) = (model, False)
