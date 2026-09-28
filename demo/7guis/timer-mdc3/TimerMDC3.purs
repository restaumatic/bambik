module TimerMDC3 (timerMDC3) where

import Prelude ((#), ($), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, updated, with)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyLarge, button, linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (elapsedFraction, nothingElapsed, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerMDC3 :: Effect Unit
timerMDC3 =
  body $
    ( Semigroupoid.do
      linearProgress @"Elapsed" elapsedFraction # shown
      (bodyLarge $ text progressLine) # shown
      sliderLive @"Duration" {}
      every tickPeriod tick
      button @"Reset" { icon: "replay" } # with nothingElapsed # updated (match { "Reset": const })
    ) # mvu tenSecondFreshTimer
