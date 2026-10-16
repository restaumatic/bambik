module TicTacToe (Board, Cell, Game, Mark, cellMark, cells, claimCell, gameOutcome, openingPosition, toMoveLine, wonLine) where

import Prelude ((&&), (/=), (<#>), (<>), (==), bind, mod, not, show)

import Data.Array (catMaybes, elem, filter, findMap, index, length, range, updateAt)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Variant (match)

type Game = { board :: Board }

type Board = Array Mark

type Mark = [ x :: {}, o :: {}, free :: {} ]

type Cell = { key :: String, line :: [ plain :: {}, winning :: {} ], mark :: Mark }

openingPosition :: Game
openingPosition =
  { board:
    [ .free {}, .free {}, .free {}
    , .free {}, .free {}, .free {}
    , .free {}, .free {}, .free {}
    ]
  }

cells :: Game -> Array Cell
cells { board } =
  let winners = fromMaybe [] (winningLine board)
  in range 0 8 <#> \i -> { key: show i, mark: fromMaybe (.free {}) (index board i), line: if i `elem` winners then .winning {} else .plain {} }

cellMark :: Cell -> String
cellMark { mark } = markText mark

markText :: Mark -> String
markText = match { x: \_ -> "X", o: \_ -> "O", free: \_ -> "" }

claimCell :: { event :: String, model :: Game } -> Game
claimCell { event: key, model: game@{ board } } = case fromString key of
  Just i | index board i == Just (.free {}) && isNothing (winningLine board) ->
    game { board = fromMaybe board (updateAt i (playerToMove board) board) }
  _ -> game

playerToMove :: Board -> Mark
playerToMove board = if length (filter (_ == .free {}) board) `mod` 2 == 1 then .x {} else .o {}

lines :: Array (Array Int)
lines =
  [ [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ]
  , [ 0, 3, 6 ], [ 1, 4, 7 ], [ 2, 5, 8 ]
  , [ 0, 4, 8 ], [ 2, 4, 6 ]
  ]

winningLine :: Board -> Maybe (Array Int)
winningLine board = findMap taken lines
  where
  taken line = case catMaybes (line <#> index board) of
    [ a, b, c ] | a == b && b == c && a /= .free {} -> Just line
    _ -> Nothing

winner :: Board -> Maybe Mark
winner board = do
  line <- winningLine board
  i <- index line 0
  index board i

boardFull :: Board -> Boolean
boardFull board = not ((.free {}) `elem` board)

gameOutcome :: Game -> [ drawn :: {}, toMove :: {}, won :: {} ]
gameOutcome { board } = case winner board of
  Just _ -> .won {}
  Nothing -> if boardFull board then .drawn {} else .toMove {}

wonLine :: Game -> String
wonLine { board } = maybe "" (\mark -> markText mark <> " wins") (winner board)

toMoveLine :: Game -> String
toMoveLine { board } = markText (playerToMove board) <> " to move"

