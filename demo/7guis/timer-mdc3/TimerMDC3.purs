module TimerMDC3 (timerMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (applied, every, mvu, state)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyLarge, button, linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerMDC3 :: Effect Unit
timerMDC3 =
  body $
    ( Semigroupoid.do
      state @"elapsed" @Number
      linearProgress @"Elapsed" elapsedFraction # shown
      (bodyLarge $ text progressLine) # shown
      sliderLive @"Duration" {}
      every tickPeriod tick
      button @"Reset" { icon: "replay" } # applied restarted
    ) # mvu tenSecondFreshTimer
