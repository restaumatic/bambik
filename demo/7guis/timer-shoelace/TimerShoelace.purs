module TimerShoelace (timerShoelace) where

import Prelude ((>>>), (#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, completed, every, projection, mvu, projected, updated)
import PUI.Web.HTML (body, p, staticText, text)
import PUI.Web.Shoelace (button, card, progressBar, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerShoelace :: Effect Unit
timerShoelace =
  body $
    card { caption: "Timer" } $ ( Semigroupoid.do
        ( RecordToRecord.do
            progressBar @"fraction" # projected fraction
            p RecordToRecord.do
              text @"elapsed" # projection show
              staticText "s / "
              text @"duration" # projection wholeSeconds
              staticText "s"
            sliderLive @"duration" {}) # completed
        every tickPeriod tick
        announce nothingElapsed >>> button @"reset" { label: "Reset" } # updated (match { reset: const })
    ) # mvu tenSecondFreshTimer
