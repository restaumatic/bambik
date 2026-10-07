module TicTacToeViewModel (cellClaimedLine, cellMark, cells, claimCell, drawnLine, gameOutcome, newGameLine, openingPosition, toMoveLine, wonLine) where

import Prelude ((&&), (/=), (<#>), (<>), (==), bind, mod, not, show)

import Data.Array (catMaybes, elem, filter, findMap, index, length, range, updateAt)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Variant (match)

openingPosition :: { board :: Array [ free :: {}, o :: {}, x :: {} ] }
openingPosition =
  { board:
    [ .free {}, .free {}, .free {}
    , .free {}, .free {}, .free {}
    , .free {}, .free {}, .free {}
    ]
  }

cells
  :: { board :: Array [ free :: {}, o :: {}, x :: {} ] }
  -> Array { key :: String
           , line :: [ plain :: {}, winning :: {} ]
           , mark :: [ free :: {}, o :: {}, x :: {} ]
           }
cells { board } =
  let winners = fromMaybe [] (winningLine board)
  in range 0 8 <#> \i -> { key: show i, mark: fromMaybe (.free {}) (index board i), line: if i `elem` winners then .winning {} else .plain {} }

cellMark
  :: { key :: String
     , line :: [ plain :: {}, winning :: {} ]
     , mark :: [ free :: {}, o :: {}, x :: {} ]
     }
  -> String
cellMark { mark } = markText { mark }

markText :: forall r1. { mark :: [ x :: {}, o :: {}, free :: {} ] | r1 } -> String
markText { mark } = match { x: \_ -> "X", o: \_ -> "O", free: \_ -> "" } mark

claimCell
  :: { event :: String, model :: { board :: Array [ free :: {}, o :: {}, x :: {} ] } }
  -> { board :: Array [ free :: {}, o :: {}, x :: {} ] }
claimCell { event: key, model: game@{ board } } = case fromString key of
  Just i | index board i == Just (.free {}) && isNothing (winningLine board) ->
    game { board = fromMaybe board (updateAt i (playerToMove board) board) }
  _ -> game

cellClaimedLine :: { board :: Array [ free :: {}, o :: {}, x :: {} ] } -> String
cellClaimedLine game = match { won: wonLine, drawn: drawnLine, toMove: toMoveLine } (gameOutcome game)

newGameLine :: { board :: Array [ free :: {}, o :: {}, x :: {} ] } -> String
newGameLine game = "New game, " <> toMoveLine { mark: playerToMove game.board }

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
boardFull board = not ((.free {}) `elem` board)

gameOutcome
  :: { board :: Array [ free :: {}, o :: {}, x :: {} ] }
  -> [ drawn :: {}
     , toMove :: { mark :: [ free :: {}, o :: {}, x :: {} ] }
     , won :: { mark :: [ free :: {}, o :: {}, x :: {} ] }
     ]
gameOutcome { board } = case winner board of
  Just m -> .won { mark: m }
  Nothing -> if boardFull board then .drawn {} else .toMove { mark: playerToMove board }

wonLine :: { mark :: [ free :: {}, o :: {}, x :: {} ] } -> String
wonLine r = markText r <> " wins"

toMoveLine :: { mark :: [ free :: {}, o :: {}, x :: {} ] } -> String
toMoveLine r = markText r <> " to move"

drawnLine :: {} -> String
drawnLine _ = "Draw"
