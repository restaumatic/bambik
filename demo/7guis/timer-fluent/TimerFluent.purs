module TimerFluent (timerFluent) where

import Prelude ((#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, every, forField, mvu, projected, updated, with)
import PUI.Web.Fluent (body1, button, card, progressBar, slider)
import PUI.Web.HTML (body, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerFluent :: Effect Unit
timerFluent =
  body $
    card { caption: "Timer" } $ ( Semigroupoid.do
        ( RecordToRecord.do
            progressBar # projected fraction
            body1 RecordToRecord.do
              text # forField @"elapsed" show
              staticText "s / "
              text # forField @"duration" wholeSeconds
              staticText "s"
            slider { label: "Duration" } # asField @"duration") # completed
        every tickPeriod tick
        button { label: "Reset" } # with nothingElapsed # updated (match { clicked: const })
    ) # mvu tenSecondFreshTimer
