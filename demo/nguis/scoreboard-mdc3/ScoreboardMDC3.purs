module ScoreboardMDC3 (scoreboardMDC3) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (accumulated, blankStatus, fold, foreach, looped, muted, ticks, with)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ScoreboardViewModel (boardSummary, gameStart, goal, scoreLine, summaryLine, tick, tickPeriod)

scoreboardMDC3 :: Effect Unit
scoreboardMDC3 =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      list $ ( listItem $ text scoreLine ) # shown # accumulated @String @{ team :: String, points :: Int } goal
      ( bodyMedium $ text summaryLine # shown ) # foreach @"key"
        @( key :: String
         , teams :: Int
         , leader :: [ led :: { team :: String, points :: Int }, unled :: {} ]
         ) boardSummary # muted ) # shown
    blankStatus @"Points scored" # ticks tickPeriod
    blankStatus @"Points scored" # fold tick
  # looped @( beat :: Int ) # with gameStart
