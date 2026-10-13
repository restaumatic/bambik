module TimerMDC2 (timerMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, looped, prefixed, ticks, unshown, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, body1, buttonWith, linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerMDC2 :: Effect Unit
timerMDC2 =
  body $ Semigroupoid.do
    prefixed ( RecordToRecord.do
      linearProgress elapsedFraction
      body1 $ text progressLine ) $ sliderLive @"Duration"
    RecordToVariant.do
      unshown @"Clock ticked" # ticks tickPeriod
      buttonWith @"Reset" { icon: "replay" }
    VariantToRecord.do
      unshown @"Clock ticked" # fold tick
      unshown @"Reset" # fold restarted
  # looped
    @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , elapsed :: Number
     ) # with tenSecondFreshTimer
