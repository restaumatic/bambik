module TimerShoelace (timerShoelace) where

import Prelude ((#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, every, forField, mvu, projected, updated, with)
import PUI.Web.HTML (body, p, staticText, text)
import PUI.Web.Shoelace (button, card, progressBar, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerShoelace :: Effect Unit
timerShoelace =
  body $
    card { caption: "Timer" } $ ( Semigroupoid.do
        ( RecordToRecord.do
            progressBar # projected fraction
            p RecordToRecord.do
              text # forField @"elapsed" show
              staticText "s / "
              text # forField @"duration" wholeSeconds
              staticText "s"
            sliderLive { label: "Duration" } # asField @"duration") # completed
        every tickPeriod tick
        button { label: "Reset" } # with nothingElapsed # updated (match { clicked: const })
    ) # mvu tenSecondFreshTimer
