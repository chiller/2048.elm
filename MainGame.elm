module MainGame where

import Graphics.Element exposing (Element)
import Keyboard
import Char
import Random
import Game exposing (myshow, gameState)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import GameUtil exposing (range, zip)

boxsize = 25
boxpadding = 5

rectAt : Int -> Int -> Svg
rectAt atx aty = rect [
    Svg.Attributes.style "fill:green;",
    x (toString atx),
    y (toString aty),
    width (toString boxsize),
    height (toString boxsize)] []

textAt : Int -> Int -> Int -> Svg
textAt atx aty val = text' [
        fontSize "8",
        textAnchor "middle",
        x (toString (atx + floor (boxsize/2))),
        y (toString (aty + floor (boxsize/2)))
    ] [text (toString val)]

gridRow : ((List Int), Int) -> List Svg
gridRow (values, aty) = range 0 ((List.length values) - 1)
        |> List.map (\x -> boxpadding + x * (boxsize + boxpadding))
        |> zip values
        |> List.concatMap (\ (val, x) -> [rectAt x aty, textAt x aty val])

grid : (List (List Int)) -> List Svg
grid matrix = range 0 ((List.length matrix) - 1)
        |> List.map (\y -> boxpadding + y * (boxsize + boxpadding))
        |> zip matrix
        |> List.concatMap gridRow

gridBackground : Int -> Svg
gridBackground size = let
      dim = boxpadding  + (boxsize + boxpadding) * size
    in
       rect [
        x "0", y "0", width (toString dim), height (toString dim),
        Svg.Attributes.style "fill:blue;"
       ] []

view gameState =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95" ]
      <| List.concat [[gridBackground 4] , grid gameState.board ]

main =
  Signal.map view gameState
