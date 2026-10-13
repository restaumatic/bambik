module Timer.Bootstrap (timer) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, looped, ticks, unshown, with)
import PUI.Web.Bootstrap (body, button, progress, sliderLive)
import PUI.Web (text)
import PUI.Web.HTML (p)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timer :: Effect Unit
timer =
  body $ Semigroupoid.do
    progress elapsedFraction
    p $ text progressLine
    sliderLive @"Duration"
    RecordToVariant.do
      unshown @"Clock ticked" # ticks tickPeriod
      button @"Reset"
    VariantToRecord.do
      unshown @"Clock ticked" # fold tick
      unshown @"Reset" # fold restarted
  # looped
    @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , elapsed :: Number
     ) # with tenSecondFreshTimer
