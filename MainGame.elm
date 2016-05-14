module MainGame where

import Graphics.Element exposing (Element)
import Keyboard
import Char
import Random
import Game exposing (myshow, gameState)
import View exposing (view)

main =
  Signal.map view gameState
