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
{-
main :: IO ()
main =
  runReactive
    (move (V2 400 500) . alignHV (0.5, 1) . viewFocusableProof mstyle)
    (nothingFocused exampleProof)
-}

data Example
  = Str Bool String
  | Node Bool Example Example

exampleWidget :: Example -> React Example
exampleWidget (Str False name) =
    React.onEventPre (React.filterInside reactive handleClick) reactive
  where
    reactive = Reactive.constant Nothing (text mstyle name)
    handleClick (MouseInput (MousePress _ MBLeft)) = Just (Str True name)
    handleClick _ = Nothing
exampleWidget (Str True name) =
    React.onEventPre handleClickOutside reactive
  where
    reactive = Reactive.constant Nothing (addBackground lightBlue (text mstyle name))
    handleClickOutside event@(MouseInput (MousePress _ MBLeft))
      | not (Reactive.eventInside reactive event) = Just (Str False name)
    handleClickOutside _ = Nothing
exampleWidget (Node False exL exR) =
    Reactive.attachFormTo right (text mstyle " ⊾ ") reactiveL
    `React.besidesRight` reactiveR
  where
    reactiveL = fmap (flip (Node False) exR) <$> exampleWidget exL
    reactiveR = fmap (Node False exL) <$> exampleWidget exR


main :: IO ()
main = runReactive
  (move (V2 400 500) . alignHV (0.5, 1) . React.toReactive exampleWidget)
  (Node False (Str False "x") (Str False "y"))
