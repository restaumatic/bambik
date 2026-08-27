module ScoreboardLogic (boardSummary, gameStart, goal, standing, tick, tickPeriod) where

import Prelude (show, (+), (==), mod)

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

goal :: { n :: Int } -> { key :: String, value :: { team :: String, points :: Int } }
goal { n } =
  let team = pick teams n
  in { key: team, value: { team, points: scored team n } }

scored :: String -> Int -> Int
scored team n = length (filter (\i -> pick teams i == team) (range 0 n))

boardSummary :: Array { team :: String, points :: Int } -> Array { key :: String, teams :: String, leader :: Maybe { team :: String, points :: Int } }
boardSummary scores = [ { key: "summary", teams: show (length scores), leader: maximumBy (comparing _.points) scores } ]

standing :: { leader :: Maybe { team :: String, points :: Int } } -> [ led :: { team :: String, points :: Int }, unled :: {} ]
standing { leader } = case leader of
  Just top -> .led top
  Nothing -> .unled {}

pick :: Array String -> Int -> String
pick options i = fromMaybe "" (index options (i `mod` length options))

teams :: Array String
teams = [ "Owls", "Foxes", "Herons" ]
