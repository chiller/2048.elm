module Game exposing (..)

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

defaultGame : (Game, Cmd Direction)
defaultGame = ({
  score = 0,
  seed = (Random.initialSeed 123),
  rand= 1,
  board = [[0,1,0,1], [0,0,0,0], [0,0,0,0], [0,0,0,0]]}, Cmd.none)

keyToDirection : Int -> Direction
keyToDirection key =
  case (Char.fromCode key) of
    'w' -> Up
    'a' -> Left
    's' -> Down
    'd' -> Right
    _ -> None


-- UPDATE
randomNumber:  Random.Generator Int
randomNumber = Random.int 0 100

update : Direction -> Game -> (Game, Cmd Direction)
update input game =
  let
    (newrand, newseed) = Random.step randomNumber game.seed
    (board, scoreDelta) = updateBoard input game.board
    newboard = spawnTile newrand 1 board
  in case newboard of
    Just board ->
    ({ game |
      score = scoreDelta + game.score,
      board = board,
      seed = newseed,
      rand = newrand
    }, Cmd.none)
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
mshiftM = compose shift ( mergeAndScore << shift )

shiftleftM : Board -> M Row
shiftleftM = flatScore << (List.map mshiftM)

shiftrightM : Board -> M Row
shiftrightM = flatScore << List.map ( compose List.reverse ( mshiftM << List.reverse))

shiftupM : Board -> M Row
shiftupM = compose transpose (shiftleftM << transpose)

shiftdownM : Board -> M Row
shiftdownM = compose transpose (shiftrightM << transpose)

updateBoard : Direction -> Board -> (Board, Score)
updateBoard input board = case input of
    Left -> shiftleftM board
    Right -> shiftrightM board
    Up -> shiftupM board
    Down -> shiftdownM board
    _ -> (board, 0)

-- SUBSCRIPTIONS

subscriptions: Game -> Sub Direction
subscriptions game = Keyboard.presses keyToDirection

