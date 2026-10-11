module TimerMDC3 (timerMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (blankStatus, fold, looped, ticks, with)
import PUI.Pipeline as Pipeline
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyLarge, button, linearProgress, sliderLive)
import TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerMDC3 :: Effect Unit
timerMDC3 =
  body $ Pipeline.do
    linearProgress elapsedFraction
    bodyLarge $ text progressLine
    sliderLive @"Duration" {}
    RecordToVariant.do
      blankStatus @"Clock ticked" # ticks tickPeriod
      button @"Reset" { icon: "replay" }
    VariantToRecord.do
      blankStatus @"Clock ticked" # fold tick
      blankStatus @"Reset" # fold restarted
  # looped
    @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , elapsed :: Number
     ) # with tenSecondFreshTimer
