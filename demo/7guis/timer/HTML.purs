module Timer.HTML (timer) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, looped, ticks, unshown, with)
import PUI.Web (staticText, text)
import PUI.Web.HTML (body, button, div, label, p, progress, rangeInput)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timer :: Effect Unit
timer =
  body $ div $ Semigroupoid.do
    progress elapsedFraction
    p $ text progressLine
    p $ label $ Semigroupoid.do
      staticText "Duration "
      rangeInput @"Duration"
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
