module TicTacToe (ticTacToe) where

import Prelude ((#), ($), (&&), (/=), (<#>), (<>), (==), Unit, bind, mod, show)

import Data.Array (catMaybes, elem, filter, findMap, index, length, range, updateAt)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, forValue, mvu, projection, updates)
import PUI.HTML (Markup(..), body, text, view)
import PUI.MDC (button, card, elevation20, headline6)
import PUI.Web (onKeyClick)
import QualifiedDo.Semigroupoid as Semigroupoid

ticTacToe :: Effect Unit
ticTacToe =
  body $
    elevation20 $
      card { caption: "Tic-Tac-Toe" } $ ( Semigroupoid.do
          headline6 (text # projection standing # forValue) # completed
          view
            """<div style="display: inline-block; margin-bottom: 10px;"></div>"""
            renderBoard
            (\node emit -> onKeyClick node \key -> emit (.cellPicked key))
            # updates (match { cellPicked: claimCell })
          button { label: "New game", icon: "replay" } # updates (match { clicked: \g _ -> startOver g })
      ) # mvu openingPosition

renderBoard :: Match -> Array Markup
renderBoard game =
  [ Element "div"
      [ Tuple "style" "display: grid; grid-template-columns: repeat(3, 72px); gap: 4px;" ]
      (range 0 8 <#> cell)
  ]
  where
  winners = fromMaybe [] (winningLine game.board)
  cell i =
    Element "div"
      [ Tuple "data-key" (show i)
      , Tuple "class" "cell"
      , Tuple "style"
          ( "height: 72px; display: flex; align-items: center; justify-content: center; "
              <> "font-size: 40px; font-family: Roboto, sans-serif; cursor: pointer; border-radius: 4px; "
              <> if i `elem` winners then "background: #a5d6a7;" else "background: #eceff1;"
          )
      ]
      [ Text (fromMaybe "" (index game.board i)) ]

type Match = { board :: Array String }

openingPosition :: Match
openingPosition = { board: [ "", "", "", "", "", "", "", "", "" ] }

startOver :: Match -> Match
startOver _ = openingPosition

claimCell :: String -> Match -> Match
claimCell key game = case fromString key of
  Just i | index game.board i == Just "" && isNothing (winningLine game.board) ->
    game { board = fromMaybe game.board (updateAt i (playerToMove game.board) game.board) }
  _ -> game

playerToMove :: Array String -> String
playerToMove board = if length (filter (_ == "") board) `mod` 2 == 1 then "X" else "O"

lines :: Array (Array Int)
lines =
  [ [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ]
  , [ 0, 3, 6 ], [ 1, 4, 7 ], [ 2, 5, 8 ]
  , [ 0, 4, 8 ], [ 2, 4, 6 ]
  ]

winningLine :: Array String -> Maybe (Array Int)
winningLine board = findMap taken lines
  where
  taken line = case catMaybes (line <#> index board) of
    [ a, b, c ] | a == b && b == c && a /= "" -> Just line
    _ -> Nothing

winner :: Array String -> Maybe String
winner board = do
  line <- winningLine board
  i <- index line 0
  index board i

boardFull :: Array String -> Boolean
boardFull board = isNothing (findMap (\m -> if m == "" then Just m else Nothing) board)

standing :: Match -> String
standing game = case winner game.board of
  Just p -> p <> " wins"
  Nothing ->
    if boardFull game.board then "Draw"
    else playerToMove game.board <> " to move"
