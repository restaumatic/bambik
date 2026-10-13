module TimerMDC3 (timerMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, looped, prefixed, ticks, unshown, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyLarge, button, linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerMDC3 :: Effect Unit
timerMDC3 =
  body $ Semigroupoid.do
    prefixed ( RecordToRecord.do
      linearProgress elapsedFraction
      bodyLarge $ text progressLine ) $ sliderLive @"Duration" {}
    RecordToVariant.do
      unshown @"Clock ticked" # ticks tickPeriod
      button @"Reset" { icon: "replay" }
    VariantToRecord.do
      unshown @"Clock ticked" # fold tick
      unshown @"Reset" # fold restarted
  # looped
    @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , elapsed :: Number
     ) # with tenSecondFreshTimer
