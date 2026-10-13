module TimerFluent (timerFluent) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, looped, prefixed, ticks, unshown, with)
import PUI.Web.Fluent (body, body1, button, progressBar, slider)
import PUI.Web (text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerFluent :: Effect Unit
timerFluent =
  body $ Semigroupoid.do
    prefixed ( RecordToRecord.do
      progressBar elapsedFraction
      body1 $ text progressLine ) $ slider @"Duration"
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
