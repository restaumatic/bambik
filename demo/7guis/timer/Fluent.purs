module Timer.Fluent (timer) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, looped, ticks, with)
import PUI.Web.Fluent (body, body1, button, progressBar, slider)
import PUI.Web (text)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timer :: Effect Unit
timer =
  body $ Semigroupoid.do
    progressBar elapsedFraction
    body1 $ text progressLine
    slider @"Duration"
    RecordToVariant.do
      ticks @"Clock ticked" tickPeriod
      button @"Reset"
    VariantToRecord.do
      fold @"Clock ticked" tick
      fold @"Reset" restarted
  # looped
    @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , elapsed :: Number
     ) # with tenSecondFreshTimer
