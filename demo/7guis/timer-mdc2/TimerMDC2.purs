module TimerMDC2 (timerMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (applied, every, mvu)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body1, button, linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerMDC2 :: Effect Unit
timerMDC2 =
  body $
    ( Semigroupoid.do
      linearProgress @"Elapsed" elapsedFraction # shown
      (body1 $ text progressLine) # shown
      sliderLive @"Duration" {}
      every tickPeriod tick
      button @"Reset" { icon: "replay" } # applied restarted
    ) # mvu tenSecondFreshTimer
