module TimerHTML (timerHTML) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (blankStatus, fold, looped, prefixed, ticks, with)
import PUI.Web (staticText, text)
import PUI.Web.HTML (body, button, div, label, p, progress, rangeInput)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerHTML :: Effect Unit
timerHTML =
  body $ div $ Semigroupoid.do
    prefixed ( RecordToRecord.do
      progress elapsedFraction
      p $ text progressLine ) $ p ( label $ prefixed (staticText "Duration ") $ rangeInput @"Duration" )
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
