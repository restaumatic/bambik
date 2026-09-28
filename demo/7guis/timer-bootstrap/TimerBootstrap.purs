module TimerBootstrap (timerBootstrap) where

import Prelude ((#), ($), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, updated, with)
import PUI.Web.Bootstrap (body, button, progress, sliderLive)
import PUI.Web (shown, text)
import PUI.Web.HTML (p)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (elapsedFraction, nothingElapsed, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerBootstrap :: Effect Unit
timerBootstrap =
  body $
    ( Semigroupoid.do
      progress @"Elapsed" elapsedFraction # shown
      (p $ text progressLine) # shown
      sliderLive @"Duration" {}
      every tickPeriod tick
      button @"Reset" {} # with nothingElapsed # updated (match { "Reset": const })
    ) # mvu tenSecondFreshTimer
