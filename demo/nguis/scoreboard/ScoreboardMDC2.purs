module ScoreboardMDC2 (scoreboardMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (accumulated, foreach)
import PUI.Web (clock, text)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ScoreboardViewModel (boardSummary, goal, scoreLine, summaryLine, tickPeriod)

scoreboardMDC2 :: Effect Unit
scoreboardMDC2 =
  body $ Semigroupoid.do
    clock @"time" tickPeriod
    list $ ( listItem $ text scoreLine ) # accumulated @String @{ team :: String, points :: Int } goal
    ( body2 $ text summaryLine ) # foreach @"key"
      @( key :: String
       , teams :: Int
       , leader :: [ led :: { team :: String, points :: Int }, unled :: {} ]
       ) boardSummary
