module Timer.Fluent (timer) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (foldInto, looped, settled, stepped, with)
import PUI.Web (text, timed, timing, timingEdits)
import PUI.Web.Fluent (body, body1, button, progressBar, slider)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, progressLine, rerun, restarted, tenSecondFreshTimer, tickPeriod)

timer :: Effect Unit
timer =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      progressBar elapsedFraction
      body1 $ text progressLine ) # timed @"time" tickPeriod
    slider @"Duration" # timingEdits @"changed" # settled rerun
    ( Semigroupoid.do
      button @"Reset" # timing @"time"
      foldInto @"Reset" restarted ) # stepped
  # looped # with tenSecondFreshTimer
