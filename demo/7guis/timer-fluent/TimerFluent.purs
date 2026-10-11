module TimerFluent (timerFluent) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (blankStatus, fold, looped, ticks, with)
import PUI.Pipeline as Pipeline
import PUI.Web.Fluent (body, body1, button, progressBar, slider)
import PUI.Web (text)
import TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerFluent :: Effect Unit
timerFluent =
  body $ Pipeline.do
    progressBar elapsedFraction
    body1 $ text progressLine
    slider @"Duration" {}
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
