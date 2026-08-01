module TicTacToeMDC2 (ticTacToeMDC2) where

import Prelude ((#), ($), (&&), (/=), (<#>), (<$>), (<>), (==), (>>>), Unit, bind, const, mod, not, show)

import Data.Array (catMaybes, elem, filter, findMap, index, length, range, updateAt)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (displayed, forField, forValue, foreach, mvu, ofField, toCase, updates, with)
import PUI.HTML (attrWith, body, clicked, div, provided, staticText, text, (:=))
import PUI.MDC2 (button, card, elevation20, headline6)
import QualifiedDo.Semigroupoid as Semigroupoid

ticTacToeMDC2 :: Effect Unit
ticTacToeMDC2 =
  body $
    elevation20 $
      card { caption: "Tic-Tac-Toe" } $ ( Semigroupoid.do
          headline6 ( RecordToRecord.do
              text # forValue # forField @"mark"
              staticText " wins" ) # provided winningMark # displayed
          headline6 (staticText "Draw") # provided drawnGame # displayed
          headline6 ( RecordToRecord.do
              text # forValue # forField @"mark"
              staticText " to move" ) # provided markToMove # displayed
          ( div >>> "style" := "display: inline-block; margin-bottom: 10px;" $
              ( div >>> "style" := "display: grid; grid-template-columns: repeat(3, 72px); gap: 4px;" $
                  ( clicked
                      ( div
                          >>> attrWith "style" (\c -> cellStyle <> if c.win then "background: #a5d6a7;" else "background: #eceff1;")
                          $ text # ofField @"mark")) # foreach @"key" cells) # toCase @"cellPicked" _.key) # updates (match { cellPicked: claimCell })
          with openingPosition (button { label: "New game", icon: "replay" }) # updates (match { clicked: const })
      ) # mvu openingPosition

cells :: { board :: Array [ x :: {}, o :: {}, free :: {} ] } -> Array { key :: String, mark :: String, win :: Boolean }
cells { board } =
  let winners = fromMaybe [] (winningLine board)
  in range 0 8 <#> \i -> { key: show i, mark: maybe "" markText (index board i), win: i `elem` winners }

markText :: [ x :: {}, o :: {}, free :: {} ] -> String
markText = match { x: \_ -> "X", o: \_ -> "O", free: \_ -> "" }

cellStyle :: String
cellStyle =
  "height: 72px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 40px; font-family: Roboto, sans-serif; cursor: pointer; border-radius: 4px; "

openingPosition :: { board :: Array [ x :: {}, o :: {}, free :: {} ] }
openingPosition =
  { board:
      [ .free {}, .free {}, .free {}
      , .free {}, .free {}, .free {}
      , .free {}, .free {}, .free {}
      ]
  }

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

winningMark :: { board :: Array [ x :: {}, o :: {}, free :: {} ] } -> Maybe { mark :: String }
winningMark { board } = (\m -> { mark: markText m }) <$> winner board

drawnGame :: { board :: Array [ x :: {}, o :: {}, free :: {} ] } -> Maybe {}
drawnGame { board } = if isNothing (winner board) && boardFull board then Just {} else Nothing

markToMove :: { board :: Array [ x :: {}, o :: {}, free :: {} ] } -> Maybe { mark :: String }
markToMove { board } =
  if isNothing (winner board) && not (boardFull board) then Just { mark: markText (playerToMove board) }
  else Nothing
