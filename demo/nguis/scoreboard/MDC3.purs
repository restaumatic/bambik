module Scoreboard.MDC3 (scoreboard) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (accumulated, foreach)
import PUI.Web (clock, text)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import Scoreboard (boardSummary, goal, scoreLine, summaryLine, tickPeriod)

scoreboard :: Effect Unit
scoreboard =
  body $ Semigroupoid.do
    clock @"time" tickPeriod
    list $ ( listItem $ text scoreLine ) # accumulated @String @{ team :: String, points :: Int } goal
    ( bodyMedium $ text summaryLine ) # foreach @"key"
      @( key :: String
       , teams :: Int
       , leader :: [ led :: { team :: String, points :: Int }, unled :: {} ]
       ) boardSummary
