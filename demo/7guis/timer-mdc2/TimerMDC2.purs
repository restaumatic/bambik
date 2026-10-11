module TimerMDC2 (timerMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (blankStatus, fold, looped, ticks, with)
import PUI.Pipeline as Pipeline
import PUI.Web (text)
import PUI.Web.MDC2 (body, body1, button, linearProgress, sliderLive)
import TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerMDC2 :: Effect Unit
timerMDC2 =
  body $ Pipeline.do
    linearProgress elapsedFraction
    body1 $ text progressLine
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
