module Timer.HTML (timer) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (foldInto, looped, settled, stepped, with)
import PUI.Web (staticText, text, timed, timing, timingEdits)
import PUI.Web.HTML (body, button, div, label, p, progress, rangeInput)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, progressLine, rerun, restarted, tenSecondFreshTimer, tickPeriod)

timer :: Effect Unit
timer =
  body $ div $ Semigroupoid.do
    ( Semigroupoid.do
      progress elapsedFraction
      p $ text progressLine ) # timed @"time" tickPeriod
    p $ label $ Semigroupoid.do
      staticText "Duration "
      rangeInput @"Duration" # timingEdits @"changed" # settled rerun
    ( Semigroupoid.do
      button @"Reset" # timing @"time"
      foldInto @"Reset" restarted ) # stepped
  # looped # with tenSecondFreshTimer
