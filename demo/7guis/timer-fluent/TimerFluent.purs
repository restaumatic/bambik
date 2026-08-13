module TimerFluent (timerFluent) where

import Prelude ((>>>), (#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, asField, completed, every, forField, mvu, projected, updated)
import PUI.Web.Fluent (body1, button, card, progressBar, slider)
import PUI.Web.HTML (body, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerFluent :: Effect Unit
timerFluent =
  body $
    card { caption: "Timer" } $ ( Semigroupoid.do
        ( RecordToRecord.do
            progressBar # projected @"value" fraction
            body1 RecordToRecord.do
              text # forField @"value" @"elapsed" show
              staticText "s / "
              text # forField @"value" @"duration" wholeSeconds
              staticText "s"
            slider { label: "Duration" } # asField @"value" @"duration") # completed
        every tickPeriod tick
        announce nothingElapsed >>> button { label: "Reset" } # updated (match { clicked: const })
    ) # mvu tenSecondFreshTimer
