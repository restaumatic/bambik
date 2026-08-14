module TimerFluent (timerFluent) where

import Prelude ((>>>), (#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, completed, every, forField, mvu, projected, updated)
import PUI.Web.Fluent (body1, button, card, progressBar, slider)
import PUI.Web.HTML (body, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerFluent :: Effect Unit
timerFluent =
  body $
    card { caption: "Timer" } $ ( Semigroupoid.do
        ( RecordToRecord.do
            progressBar @"value" # projected @"value" fraction
            body1 RecordToRecord.do
              text @"value" # forField @"elapsed" show
              staticText "s / "
              text @"value" # forField @"duration" wholeSeconds
              staticText "s"
            slider @"duration" { label: "Duration" }) # completed
        every tickPeriod tick
        announce nothingElapsed >>> button { label: "Reset" } # updated (match { clicked: const })
    ) # mvu tenSecondFreshTimer
