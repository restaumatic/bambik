module TimerMDC2 (timerMDC2) where

import Prelude ((>>>), (#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, completed, every, forField, mvu, projected, updated)
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
              linearProgress @"value" # projected @"value" fraction
              body1 RecordToRecord.do
                text @"value" # forField @"elapsed" show
                staticText "s / "
                text @"value" # forField @"duration" wholeSeconds
                staticText "s"
              sliderLive @"duration" {}) # completed
          every tickPeriod tick
          announce nothingElapsed >>> button { label: "Reset", icon: "replay" } # updated (match { clicked: const })
      ) # mvu tenSecondFreshTimer
