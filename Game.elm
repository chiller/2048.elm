module Game where

import Keyboard
import Char
import Random
import Util exposing (..)

-- MODEL
type Direction = Left | Right | Up | Down | None
type alias Board = List Row
type alias Row = List Int
type alias Score = Int
type alias Game = {
  score: Score, board: Board, rand: Int,  seed: Random.Seed
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
    (board, scoreDelta) = updateBoard input game.board
    newboard = spawnTile newrand 1 board
  in case newboard of
    Just board ->
    { game |
      score = scoreDelta + game.score,
      board = board,
      seed = newseed,
      rand = newrand
    }
    Nothing -> defaultGame

spawnTile : Int -> Int -> Board -> Maybe Board
spawnTile index val board =
  let
    updatedBoard = updatezero index val <| List.concat board
  in case updatedBoard of
     Just board -> Just <| splitAt 4 <| board
     -- TODO : game ends here
     Nothing -> Nothing

updatezero :  Int -> Int -> Row -> Maybe Row
updatezero index val list =
   let
     zeros = List.length <| List.filter ( (==) 0 ) list
   in case zeros of
     0 -> Nothing
     _ -> Just (updateone (index % zeros) val list )

mshiftM : Row -> M Int
mshiftM = shift `compose` ( mergeAndScore << shift )

shiftleftM : Board -> M Row
shiftleftM = flatScore << (List.map mshiftM)

shiftrightM : Board -> M Row
shiftrightM = flatScore << List.map ( List.reverse `compose` ( mshiftM << List.reverse))

shiftupM : Board -> M Row
shiftupM = transpose `compose` (shiftleftM << transpose)

shiftdownM : Board -> M Row
shiftdownM = transpose `compose` (shiftrightM << transpose)

updateBoard : Direction -> Board -> (Board, Score)
updateBoard input board = case input of
    Left -> shiftleftM board
    Right -> shiftrightM board
    Up -> shiftupM board
    Down -> shiftdownM board
    _ -> (board, 0)

-- SIGNALS

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame direction

direction : Signal Direction
direction = Signal.map keyToDirection Keyboard.presses
