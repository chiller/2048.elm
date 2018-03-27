module MainGame exposing (..)

import Html
import Game exposing (defaultGame, update, subscriptions)
import View exposing (view)

main = Html.program {
    init = defaultGame
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



