module TimerMDC3 (timerMDC3) where

import Prelude ((>>>), (#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, completed, every, projection, mvu, projected, updated)
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
              linearProgress @"fraction" # projected fraction
              bodyLarge RecordToRecord.do
                text @"elapsed" # projection show
                staticText "s / "
                text @"duration" # projection wholeSeconds
                staticText "s"
              sliderLive @"duration" {}) # completed
          every tickPeriod tick
          announce nothingElapsed >>> button @"reset" { label: "Reset", icon: "replay" } # updated (match { reset: const })
      ) # mvu tenSecondFreshTimer
