module TimerBootstrap (timerBootstrap) where

import Prelude ((>>>), (#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, asField, completed, every, forField, mvu, projected, updated)
import PUI.Web.Bootstrap (button, card, progress, sliderLive)
import PUI.Web.HTML (body, p, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerBootstrap :: Effect Unit
timerBootstrap =
  body $
    card { caption: "Timer" } $ ( Semigroupoid.do
        ( RecordToRecord.do
            progress # projected @"value" fraction
            p RecordToRecord.do
              text # forField @"elapsed" show
              staticText "s / "
              text # forField @"duration" wholeSeconds
              staticText "s"
            sliderLive { label: "Duration" } # asField @"value" @"duration") # completed
        every tickPeriod tick
        announce nothingElapsed >>> button { label: "Reset" } # updated (match { clicked: const })
    ) # mvu tenSecondFreshTimer
