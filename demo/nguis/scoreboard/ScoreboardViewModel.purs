module ScoreboardViewModel (boardSummary, gameStart, goal, pointsScoredLine, scoreLine, summaryLine, tick, tickPeriod) where

import Prelude (const, show, (+), (<>), (==), mod)

import Data.Array (filter, index, length, range)
import Data.Foldable (maximumBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (comparing)
import Data.Variant (match)

gameStart :: { beat :: Int }
gameStart = { beat: 0 }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

tick :: { beat :: Int } -> { beat :: Int }
tick { beat } = { beat: beat + 1 }

pointsScoredLine :: { beat :: Int } -> String
pointsScoredLine { beat } = pick teams (beat + 1) <> " scored"

goal :: { beat :: Int } -> { key :: String, value :: { points :: Int, team :: String } }
goal { beat } =
  let team = pick teams beat
      points = scored team beat
  in { key: team, value: { team, points } }

scoreLine :: { points :: Int, team :: String } -> String
scoreLine { team, points } = team <> ": " <> show points

scored :: String -> Int -> Int
scored team beat = length (filter (\i -> pick teams i == team) (range 0 beat))

boardSummary :: Array { points :: Int, team :: String } -> Array { key :: String, leader :: [ led :: { points :: Int, team :: String }, unled :: {} ], teams :: Int }
boardSummary scores = [ { key: "summary", teams: length scores, leader: leaderOf scores } ]

summaryLine :: { key :: String, leader :: [ led :: { points :: Int, team :: String }, unled :: {} ], teams :: Int } -> String
summaryLine r = show r.teams <> " teams on the board — leading: " <> match { led: \{ team, points } -> team <> " (" <> show points <> ")", unled: const "—" } r.leader

leaderOf :: Array { team :: String, points :: Int } -> [ led :: { team :: String, points :: Int }, unled :: {} ]
leaderOf scores = case maximumBy (comparing _.points) scores of
  Just top -> .led { team: top.team, points: top.points }
  Nothing -> .unled {}

pick :: Array String -> Int -> String
pick options i = fromMaybe "" (index options (i `mod` length options))

teams :: Array String
teams = [ "Owls", "Foxes", "Herons" ]
