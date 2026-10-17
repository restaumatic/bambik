module Timer.Bootstrap (timer) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, looped, ticks, with)
import PUI.Web (text)
import PUI.Web.Bootstrap (body, button, progress, sliderLive)
import PUI.Web.HTML (p)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, progressLine, restarted, tenSecondFreshTimer, tick, tickPeriod)

timer :: Effect Unit
timer =
  body $ Semigroupoid.do
    progress elapsedFraction
    p $ text progressLine
    sliderLive @"Duration"
    RecordToVariant.do
      ticks @"Clock ticked" tickPeriod
      button @"Reset"
    VariantToRecord.do
      fold @"Clock ticked" tick
      fold @"Reset" restarted
  # looped # with tenSecondFreshTimer
