module TimerShoelace (timerShoelace) where

import Prelude ((>>>), (#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, completed, every, forField, mvu, projected, updated)
import PUI.Web.HTML (body, p, staticText, text)
import PUI.Web.Shoelace (button, card, progressBar, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerShoelace :: Effect Unit
timerShoelace =
  body $
    card { caption: "Timer" } $ ( Semigroupoid.do
        ( RecordToRecord.do
            progressBar @"value" # projected @"value" fraction
            p RecordToRecord.do
              text @"value" # forField @"elapsed" show
              staticText "s / "
              text @"value" # forField @"duration" wholeSeconds
              staticText "s"
            sliderLive @"duration" { label: "Duration" }) # completed
        every tickPeriod tick
        announce nothingElapsed >>> button { label: "Reset" } # updated (match { clicked: const })
    ) # mvu tenSecondFreshTimer
