module Scoreboard.MDC2 (scoreboard) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (accumulated, foreach)
import PUI.Web (clock, text)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import Scoreboard (boardSummary, goal, scoreLine, summaryLine, tickPeriod)

scoreboard :: Effect Unit
scoreboard =
  body $ Semigroupoid.do
    clock @"time" tickPeriod
    list $ ( listItem $ text scoreLine ) # accumulated goal
    ( body2 $ text summaryLine ) # foreach @"key" boardSummary
