module MainGame where

import Graphics.Element exposing (Element)
import Keyboard
import Char
import Random
import Game exposing (myshow, gameState)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import GameUtil exposing (range)

boxsize = 25
boxpadding = 5

rectAt : Int -> Int -> Svg
rectAt atx aty = rect [
    x (toString atx),
    y (toString aty),
    width (toString boxsize),
    height (toString boxsize)] []

gridRow : Int -> Int -> List Svg
gridRow size aty = List.map (\ x -> rectAt x aty)
        <| List.map (\x -> boxpadding + x * (boxsize + boxpadding))
        <| range 0 (size - 1)

grid : Int -> List Svg
grid size = range 0 (size - 1)
        |> List.map (\x -> boxpadding + x * (boxsize + boxpadding))
        |> List.map (gridRow 4)
        |> List.concat

gridBackground : Int -> Svg
gridBackground size = let
      dim = boxpadding  + (boxsize + boxpadding) * size
    in
       rect [
        x "0", y "0", width (toString dim), height (toString dim),
        Svg.Attributes.style "fill:blue;"
       ] []

view _ =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95" ]
      <| List.concat [[gridBackground 4] , grid 4]

main =
  Signal.map view gameState
