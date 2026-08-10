module TimerMDC2 (timerMDC2) where

import Prelude ((#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, every, forField, mvu, projected, updated, with)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerMDC2 :: Effect Unit
timerMDC2 =
  body $
    elevation20 $
      card { caption: "Timer" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              linearProgress # projected @"value" fraction
              body1 RecordToRecord.do
                text # forField @"value" @"elapsed" show
                staticText "s / "
                text # forField @"value" @"duration" wholeSeconds
                staticText "s"
              sliderLive { label: "" } # asField @"value" @"duration") # completed
          every tickPeriod tick
          button { label: "Reset", icon: "replay" } # with nothingElapsed # updated (match { clicked: const })
      ) # mvu tenSecondFreshTimer
