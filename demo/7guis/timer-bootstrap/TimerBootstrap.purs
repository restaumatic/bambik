module TimerBootstrap (timerBootstrap) where

import Prelude ((#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, every, projection, mvu, projected, updated, with)
import PUI.Web.Bootstrap (button, card, progress, sliderLive)
import PUI.Web.HTML (body, p, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerBootstrap :: Effect Unit
timerBootstrap =
  body $
    card { caption: "Timer" } $ ( Semigroupoid.do
        ( RecordToRecord.do
            progress @"fraction" # projected fraction
            p RecordToRecord.do
              text @"elapsed" # projection show
              staticText "s / "
              text @"duration" # projection wholeSeconds
              staticText "s"
            sliderLive @"duration" {}) # completed
        every tickPeriod tick
        button @"reset" { label: "Reset" } # with nothingElapsed # updated (match { reset: const })
    ) # mvu tenSecondFreshTimer
