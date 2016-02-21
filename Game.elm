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
import Html exposing (text)
import Random
import GameUtil exposing (..)

-- MODEL 
type Direction = Left | Right | Up | Down | None
type alias Board = Row  
type alias Row = List Int
type alias Game = {
  score: Int, board: Board, rand: Int,  seed: Random.Seed
}

defaultGame = { 
  score = 0, 
  seed = (Random.initialSeed 123), 
  rand= 1, 
  board = [0,1,0,1]}

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

updatezero :  Int -> Int -> List Int -> Maybe (List Int)
updatezero index with list = 
   let 
     zeros = List.length <| List.filter ( (==) 0 ) list
   in 
     if (zeros == 0) then Nothing else Just (updateone (index % zeros) with list )
   

updateBoard input board = 
  case input of 
    Left -> mshift board 
    Right -> List.reverse << mshift << List.reverse <| board 
    _ -> board
    
updateScore input score = score

zeroes = List.filter <| (==) 0
nonzeroes = List.filter <| (/=) 0 

shift : List Int -> List Int
shift board = nonzeroes board ++ zeroes board

mshift = shift << merge << shift

merge list = case list of
    (x::y::xs) -> 
      if (x == y) then [x*2, 0] ++ merge xs
      else x :: merge (y::xs)
    xs -> xs
-- VIEW

myshow x = show x

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




