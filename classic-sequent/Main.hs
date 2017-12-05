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
import Linear
import FormUtils
import Data.Functor.Foldable
import Data.Maybe
import qualified React
import React (React)

import Sequent
import TermWidget
import ProofWidget

mstyle :: TextStyle
mstyle = defaultTextStyle { fontSize = 20 }

main :: IO ()
main =
  runReactive
    (React.toReactive . move (V2 400 500) . alignHV (0.5, 1) . viewFocusableProof mstyle)
    (nothingFocused exampleProof)
