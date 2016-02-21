module MainGame where

import Graphics.Element exposing (..)
import Keyboard
import Char
import Html exposing (text)
import Random
import Game exposing (myshow, gameState)


main : Signal Element
main =
  Signal.map myshow gameState
