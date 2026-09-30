module TimerHTML (timerHTML) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (applied, every, mvu)
import PUI.Web (shown, staticText, text)
import PUI.Web.HTML (body, button, div, label, p, progress, rangeInput)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerHTML :: Effect Unit
timerHTML =
  body $ div $ ( Semigroupoid.do
    progress @"Elapsed" elapsedFraction # shown
    (p $ text progressLine) # shown
    p ( label $ Semigroupoid.do
      (staticText @"Duration ") # shown
      rangeInput @"Duration" )
    every tickPeriod tick
    button @"Reset" {} # applied restarted
  ) # mvu tenSecondFreshTimer
