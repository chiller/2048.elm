-- Example.elm
import String
import Graphics.Element exposing (Element)

import ElmTest exposing (..)
import Game exposing (..)


testUpdateBoard =
    let
      board = [[1,1], [0,0]]
      actual = updateBoard Left board
      expected = [[2,0], [0,0]]

    in test "Update Board" (assertEqual expected actual)

tests : Test
tests =
    suite "A Test Suite"
        [ testUpdateBoard
        ]


main : Element
main =
    elementRunner tests
