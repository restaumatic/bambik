module TicTacToeLogic (cellMark, cells, claimCell, gameOutcome, openingPosition, toMoveLine, wonLine) where

import Prelude ((&&), (/=), (<#>), (<>), (==), bind, mod, show)

import Data.Array (catMaybes, elem, filter, findMap, index, length, range, updateAt)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Variant (match)

openingPosition :: { board :: Array [ x :: {}, o :: {}, free :: {} ] }
openingPosition =
  { board:
      [ .free {}, .free {}, .free {}
      , .free {}, .free {}, .free {}
      , .free {}, .free {}, .free {}
      ]
  }

cells :: { board :: Array [ x :: {}, o :: {}, free :: {} ] } -> Array { key :: String, mark :: [ x :: {}, o :: {}, free :: {} ], line :: [ winning :: {}, plain :: {} ] }
cells { board } =
  let winners = fromMaybe [] (winningLine board)
  in range 0 8 <#> \i -> { key: show i, mark: fromMaybe (.free {}) (index board i), line: if i `elem` winners then .winning {} else .plain {} }

cellMark :: { mark :: [ x :: {}, o :: {}, free :: {} ], line :: [ winning :: {}, plain :: {} ] } -> String
cellMark { mark } = markText { mark }

markText :: { mark :: [ x :: {}, o :: {}, free :: {} ] } -> String
markText { mark } = match { x: \_ -> "X", o: \_ -> "O", free: \_ -> "" } mark

claimCell :: String -> { board :: Array [ x :: {}, o :: {}, free :: {} ] } -> { board :: Array [ x :: {}, o :: {}, free :: {} ] }
claimCell key game@{ board } = case fromString key of
  Just i | index board i == Just (.free {}) && isNothing (winningLine board) ->
    game { board = fromMaybe board (updateAt i (playerToMove board) board) }
  _ -> game

playerToMove :: Array [ x :: {}, o :: {}, free :: {} ] -> [ x :: {}, o :: {}, free :: {} ]
playerToMove board = if length (filter (_ == .free {}) board) `mod` 2 == 1 then .x {} else .o {}

lines :: Array (Array Int)
lines =
  [ [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ]
  , [ 0, 3, 6 ], [ 1, 4, 7 ], [ 2, 5, 8 ]
  , [ 0, 4, 8 ], [ 2, 4, 6 ]
  ]

winningLine :: Array [ x :: {}, o :: {}, free :: {} ] -> Maybe (Array Int)
winningLine board = findMap taken lines
  where
  taken line = case catMaybes (line <#> index board) of
    [ a, b, c ] | a == b && b == c && a /= .free {} -> Just line
    _ -> Nothing

winner :: Array [ x :: {}, o :: {}, free :: {} ] -> Maybe [ x :: {}, o :: {}, free :: {} ]
winner board = do
  line <- winningLine board
  i <- index line 0
  index board i

boardFull :: Array [ x :: {}, o :: {}, free :: {} ] -> Boolean
boardFull board = isNothing (findMap (\m -> if m == .free {} then Just m else Nothing) board)

gameOutcome :: { board :: Array [ x :: {}, o :: {}, free :: {} ] } -> [ won :: { mark :: [ x :: {}, o :: {}, free :: {} ] }, drawn :: {}, toMove :: { mark :: [ x :: {}, o :: {}, free :: {} ] } ]
gameOutcome { board } = case winner board of
  Just m -> .won { mark: m }
  Nothing -> if boardFull board then .drawn {} else .toMove { mark: playerToMove board }

wonLine :: { mark :: [ x :: {}, o :: {}, free :: {} ] } -> String
wonLine r = markText r <> " wins"

toMoveLine :: { mark :: [ x :: {}, o :: {}, free :: {} ] } -> String
toMoveLine r = markText r <> " to move"
