module Scoreboard (Leader, Score, Summary, boardSummary, goal, scoreLine, summaryLine, tickPeriod) where

import Prelude (const, show, (*), (+), (<>), (==), mod)

import Data.Array (filter, index, length, range)
import Data.Foldable (maximumBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (comparing)
import Data.Variant (match)

type Score = { points :: Int, team :: String }

type Summary = { key :: String, leader :: Leader, teams :: Int }

type Leader = [ led :: Score, unled :: {} ]

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

goal
  :: { time :: { hours :: Int, minutes :: Int, seconds :: Int } }
  -> { key :: String, value :: Score }
goal { time } =
  let beat = time.minutes * 60 + time.seconds
      team = pick teams beat
      points = scored team beat
  in { key: team, value: { team, points } }

scoreLine :: Score -> String
scoreLine { team, points } = team <> ": " <> show points

scored :: String -> Int -> Int
scored team beat = length (filter (\i -> pick teams i == team) (range 0 beat))

boardSummary :: Array Score -> Array Summary
boardSummary scores = [ { key: "summary", teams: length scores, leader: leaderOf scores } ]

summaryLine :: Summary -> String
summaryLine r = show r.teams <> " teams on the board — leading: " <> match { led: \{ team, points } -> team <> " (" <> show points <> ")", unled: const "—" } r.leader

leaderOf :: Array Score -> Leader
leaderOf scores = case maximumBy (comparing _.points) scores of
  Just top -> .led { team: top.team, points: top.points }
  Nothing -> .unled {}

pick :: Array String -> Int -> String
pick options i = fromMaybe "" (index options (i `mod` length options))

teams :: Array String
teams = [ "Owls", "Foxes", "Herons" ]
