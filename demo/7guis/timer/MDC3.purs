module Timer.MDC3 (timer) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, looped, ticks, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyLarge, button', linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timer :: Effect Unit
timer =
  body $ Semigroupoid.do
    linearProgress elapsedFraction
    bodyLarge $ text progressLine
    sliderLive @"Duration"
    RecordToVariant.do
      ticks @"Clock ticked" tickPeriod
      button' @"Reset" { icon: "replay" }
    VariantToRecord.do
      fold @"Clock ticked" tick
      fold @"Reset" restarted
  # looped # with tenSecondFreshTimer
