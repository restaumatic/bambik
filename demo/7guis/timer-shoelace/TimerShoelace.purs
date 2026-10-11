module TimerShoelace (timerShoelace) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (blankStatus, fold, looped, ticks, with)
import PUI.Pipeline as Pipeline
import PUI.Web (text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, button, progressBar, sliderLive)
import TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerShoelace :: Effect Unit
timerShoelace =
  body $ Pipeline.do
    progressBar elapsedFraction
    p $ text progressLine
    sliderLive @"Duration" {}
    RecordToVariant.do
      blankStatus @"Clock ticked" # ticks tickPeriod
      button @"Reset" {}
    VariantToRecord.do
      blankStatus @"Clock ticked" # fold tick
      blankStatus @"Reset" # fold restarted
  # looped
    @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , elapsed :: Number
     ) # with tenSecondFreshTimer
