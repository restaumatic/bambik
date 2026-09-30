module TimerBootstrap (timerBootstrap) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (applied, every, mvu)
import PUI.Web.Bootstrap (body, button, progress, sliderLive)
import PUI.Web (shown, text)
import PUI.Web.HTML (p)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerBootstrap :: Effect Unit
timerBootstrap =
  body $
    ( Semigroupoid.do
      progress @"Elapsed" elapsedFraction # shown
      (p $ text progressLine) # shown
      sliderLive @"Duration" {}
      every tickPeriod tick
      button @"Reset" {} # applied restarted
    ) # mvu tenSecondFreshTimer
