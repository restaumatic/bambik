module Scoreboard.MDC2 (scoreboard) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (accumulatedOf, actedOf, ticks)
import PUI.Web (text, timestamping)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import Scoreboard (boardSummary, clockPeriod, goal, scoreLine, summaryLine)

scoreboard :: Effect Unit
scoreboard =
  body $ Semigroupoid.do
    ticks @"Clock ticked" clockPeriod # timestamping @"time"
    list $ ( listItem $ text scoreLine ) # accumulatedOf @"team" goal
    ( body2 $ text summaryLine ) # actedOf @"key" boardSummary
