module TimerFluent (timerFluent) where

import Prelude ((#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, every, projection, mvu, projected, updated, with)
import PUI.Web.Fluent (body1, button, card, progressBar, slider)
import PUI.Web.HTML (body, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerFluent :: Effect Unit
timerFluent =
  body $
    card { caption: "Timer" } $ ( Semigroupoid.do
        ( RecordToRecord.do
            progressBar @"fraction" # projected fraction
            body1 RecordToRecord.do
              text @"elapsed" # projection show
              staticText "s / "
              text @"duration" # projection wholeSeconds
              staticText "s"
            slider @"duration" {}) # completed
        every tickPeriod tick
        button @"Reset" {} # with nothingElapsed # updated (match { "Reset": const })
    ) # mvu tenSecondFreshTimer
