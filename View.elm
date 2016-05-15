module View where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Util exposing (range, zip)
import Game exposing (Game)

boxsize = 25
boxpadding = 5

colorForVal : Int -> String
colorForVal val = logBase 2 (toFloat val) |> truncate |> \v -> "rgb("++ (toString (150 + v * 20)) ++",150,150)"

rectAt : Int -> Int -> Int -> Svg
rectAt atx aty val = rect [
    Svg.Attributes.style ("fill:"++ (colorForVal val)++";"),
    x (toString atx),
    y (toString aty),
    width (toString boxsize),
    height (toString boxsize)] []

textAt : Int -> Int -> Int -> Svg
textAt atx aty val = let
       val' = case val of
           0 -> ""
           _ -> toString val
    in text' [
        fontSize "12",
        fontFamily "'Clear Sans', 'Helvetica Neue', Arial, sans-serif",
        fontWeight "bold",
        textAnchor "middle",
        x (toString (atx + floor (boxsize/2))),
        y (toString (aty + floor (boxsize/2) + 4))
    ] [text val']

gridRow : ((List Int), Int) -> List Svg
gridRow (values, aty) = range 0 ((List.length values) - 1)
        |> List.map (\x -> boxpadding + x * (boxsize + boxpadding))
        |> zip values
        |> List.concatMap (\ (val, x) -> [rectAt x aty val, textAt x aty val])

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
        Svg.Attributes.style "fill:rgb(100,100,100);"
       ] []

view : Game -> Svg
view gameState =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95" ]
      <| List.concat [[gridBackground 4] , grid gameState.board ]
