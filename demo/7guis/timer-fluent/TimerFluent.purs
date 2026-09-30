module TimerFluent (timerFluent) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (applied, every, mvu)
import PUI.Web.Fluent (body, body1, button, progressBar, slider)
import PUI.Web (shown, text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerFluent :: Effect Unit
timerFluent =
  body $
    ( Semigroupoid.do
      progressBar @"Elapsed" elapsedFraction # shown
      (body1 $ text progressLine) # shown
      slider @"Duration" {}
      every tickPeriod tick
      button @"Reset" {} # applied restarted
    ) # mvu tenSecondFreshTimer
