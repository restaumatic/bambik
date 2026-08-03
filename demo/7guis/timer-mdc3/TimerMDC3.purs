module TimerMDC3 (timerMDC3) where

import Prelude ((#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, every, forField, mvu, projected, updated, with)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerMDC3 :: Effect Unit
timerMDC3 =
  body $
    elevation5 $
      card { caption: "Timer" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              linearProgress # projected fraction
              bodyLarge RecordToRecord.do
                text # forField @"elapsed" show
                staticText "s / "
                text # forField @"duration" wholeSeconds
                staticText "s"
              sliderLive { label: "" } # asField @"duration") # completed
          every tickPeriod tick
          button { label: "Reset", icon: "replay" } # with nothingElapsed # updated (match { clicked: const })
      ) # mvu tenSecondFreshTimer
