module TimerMDC3 (timerMDC3) where

import Prelude ((>>>), (#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, completed, every, forField, mvu, projected, updated)
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
              linearProgress @"value" # projected @"value" fraction
              bodyLarge RecordToRecord.do
                text @"value" # forField @"elapsed" show
                staticText "s / "
                text @"value" # forField @"duration" wholeSeconds
                staticText "s"
              sliderLive @"duration" { label: "Duration" }) # completed
          every tickPeriod tick
          announce nothingElapsed >>> button { label: "Reset", icon: "replay" } # updated (match { clicked: const })
      ) # mvu tenSecondFreshTimer
