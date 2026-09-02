module ScoreboardLogic (boardSummary, gameStart, goal, standing, tick, tickPeriod) where

import Prelude (show, (+), (<>), (==), mod)

import Data.Array (filter, index, length, range)
import Data.Foldable (maximumBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (comparing)

gameStart :: { n :: Int }
gameStart = { n: 0 }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

tick :: { n :: Int } -> Maybe { n :: Int }
tick { n } = Just { n: n + 1 }

goal :: { n :: Int } -> { key :: String, value :: { team :: String, points :: Int, scoreLine :: String } }
goal { n } =
  let team = pick teams n
      points = scored team n
  in { key: team, value: { team, points, scoreLine: team <> ": " <> show points } }

scored :: String -> Int -> Int
scored team n = length (filter (\i -> pick teams i == team) (range 0 n))

boardSummary :: Array { team :: String, points :: Int, scoreLine :: String } -> Array { key :: String, teamsLine :: String, leader :: [ led :: { leaderLine :: String }, unled :: {} ] }
boardSummary scores = [ { key: "summary", teamsLine: show (length scores) <> " teams on the board — leading: ", leader: leaderOf scores } ]

leaderOf :: Array { team :: String, points :: Int, scoreLine :: String } -> [ led :: { leaderLine :: String }, unled :: {} ]
leaderOf scores = case maximumBy (comparing _.points) scores of
  Just top -> .led { leaderLine: top.team <> " (" <> show top.points <> ")" }
  Nothing -> .unled {}

standing :: { leader :: [ led :: { leaderLine :: String }, unled :: {} ] } -> [ led :: { leaderLine :: String }, unled :: {} ]
standing { leader } = leader

pick :: Array String -> Int -> String
pick options i = fromMaybe "" (index options (i `mod` length options))

teams :: Array String
teams = [ "Owls", "Foxes", "Herons" ]
