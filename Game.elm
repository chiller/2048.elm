module Game where
{-
TODO: two dimensional
TODO: implement score
TODO: should not update board if NONE direction
TODO: should not spawn new if nothing changed
-}
import Graphics.Element exposing (..)
import Keyboard
import Char
import Html exposing (..)
import Html.Attributes exposing (class, href)

import Random
import Graphics.Collage exposing (..)
import GameUtil exposing (..)

-- MODEL 
type Direction = Left | Right | Up | Down | None
type alias Board = List Row  
type alias Row = List Int
type alias Game = {
  score: Int, board: Board, rand: Int,  seed: Random.Seed
}

defaultGame = { 
  score = 0, 
  seed = (Random.initialSeed 123), 
  rand= 1, 
  board = [[0,1,0,1], [0,0,0,0], [0,0,0,0], [0,0,0,0]]}

-- UPDATE
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

addZero newrand list = updatezero newrand 1 list 
                       
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
    
updateScore input score = score

-- VIEW

myshow state = show state -- collage 200 200 [toForm ( show state.board ) ]

-- SIGNALS


gameState : Signal Game
gameState =
  Signal.foldp update defaultGame direction
  
arrowMap key = 
  case (Char.fromCode key) of 
    'w' -> Up
    'a' -> Left
    's' -> Down
    'd' -> Right
    _ -> None

direction : Signal Direction
direction = Signal.map arrowMap Keyboard.presses



