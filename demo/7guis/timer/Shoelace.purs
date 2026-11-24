module Timer.Shoelace (timer) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (foldInto, looped, settled, stepped, with)
import PUI.Web (text, timed, timing, timingEdits)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, button, progressBar, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, progressLine, rerun, restarted, tenSecondFreshTimer, tickPeriod)

timer :: Effect Unit
timer =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      progressBar elapsedFraction
      p $ text progressLine ) # timed @"time" tickPeriod
    sliderLive @"Duration" # timingEdits @"changed" # settled rerun
    ( Semigroupoid.do
      button @"Reset" # timing @"time"
      foldInto @"Reset" restarted ) # stepped
  # looped # with tenSecondFreshTimer
