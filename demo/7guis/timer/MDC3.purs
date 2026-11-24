module Timer.MDC3 (timer) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (foldInto, looped, settled, stepped, with)
import PUI.Web (text, timed, timing, timingEdits)
import PUI.Web.MDC3 (body, bodyLarge, button', linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, progressLine, rerun, restarted, tenSecondFreshTimer, tickPeriod)

timer :: Effect Unit
timer =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      linearProgress elapsedFraction
      bodyLarge $ text progressLine ) # timed @"time" tickPeriod
    sliderLive @"Duration" # timingEdits @"changed" # settled rerun
    ( Semigroupoid.do
      button' @"Reset" { icon: "replay" } # timing @"time"
      foldInto @"Reset" restarted ) # stepped
  # looped # with tenSecondFreshTimer
