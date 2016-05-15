module MainGame where

import Html
import Game exposing (gameState)
import View exposing (view)

main : Signal Html.Html
main =
  Signal.map view gameState
