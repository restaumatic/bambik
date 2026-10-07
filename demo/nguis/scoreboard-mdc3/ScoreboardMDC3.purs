module ScoreboardMDC3 (scoreboardMDC3) where

import Prelude ((#), ($), Unit, identity)

import Effect (Effect)
import PUI (accumulated, fold, foreach, looped, muted, replaying, ticks, with)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import ScoreboardViewModel (boardSummary, gameStart, goal, pointsScoredLine, scoreLine, summaryLine, tick, tickPeriod)

scoreboardMDC3 :: Effect Unit
scoreboardMDC3 =
  body $
    ( Semigroupoid.do
      ( Semigroupoid.do
        list $ ( listItem $ text scoreLine ) # shown # accumulated @String @{ team :: String, points :: Int } goal
        ( bodyMedium $ text summaryLine # shown ) # foreach @"key" @( key :: String, teams :: Int, leader :: [ led :: { team :: String, points :: Int }, unled :: {} ] ) boardSummary # muted ) # shown
      ticks @"Points scored" tickPeriod # replaying @"Points scored" identity
      snackbar @"Points scored" pointsScoredLine # fold tick
    ) # looped @( beat :: Int ) # with gameStart
