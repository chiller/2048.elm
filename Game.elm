module Game where

import Keyboard
import Char
import Random
import Util exposing (..)

-- MODEL
type Direction = Left | Right | Up | Down | None
type alias Board = List Row
type alias Row = List Int
type alias Game = {
  score: Int, board: Board, rand: Int,  seed: Random.Seed
}

defaultGame : Game
defaultGame = {
  score = 0,
  seed = (Random.initialSeed 123),
  rand= 1,
  board = [[0,1,0,1], [0,0,0,0], [0,0,0,0], [0,0,0,0]]}

keyToDirection : Int -> Direction
keyToDirection key =
  case (Char.fromCode key) of
    'w' -> Up
    'a' -> Left
    's' -> Down
    'd' -> Right
    _ -> None

-- UPDATE
update : Direction -> Game -> Game
update input game =
  let
    (newrand, newseed) = Random.generate (Random.int 0 100) game.seed
    newboard = updatezero newrand 1 <| updateBoard input game.board
  in case newboard of
    Just board ->
    { game |
      score = updateScore input game.score,
      board = board,
      seed = newseed,
      rand = newrand
    }
    Nothing -> defaultGame

updatezero : Int -> Int -> Board -> Maybe Board
updatezero index with board =
  let
    updatedBoard = updatezero' index with <| List.foldr (++) [] board
  in case updatedBoard of
     Just board -> Just <| splitAt 4 <| board
     Nothing -> Nothing

updatezero' :  Int -> Int -> Row -> Maybe Row
updatezero' index with list =
   let
     zeros = List.length <| List.filter ( (==) 0 ) list
   in
     if (zeros == 0) then Nothing else Just (updateone (index % zeros) with list )

updateBoard : Direction -> Board -> Board
updateBoard input board =
  let
    mshift = shift << merge << shift
    shiftleft = List.map mshift
    shiftright = List.map ( List.reverse << mshift << List.reverse)
    shiftup = transpose << shiftleft << transpose
    shiftdown = transpose << shiftright << transpose
  in case input of
    Left -> shiftleft board
    Right -> shiftright board
    Up -> shiftup board
    Down -> shiftdown board
    _ -> board

-- TODO
updateScore : Direction -> Int -> Int
updateScore input score = score

-- SIGNALS

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame direction

direction : Signal Direction
direction = Signal.map keyToDirection Keyboard.presses



