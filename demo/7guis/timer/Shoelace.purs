module Timer.Shoelace (timer) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Effect (Effect)
import PUI (edits)
import PUI.Web (text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, button, progressBar, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, progressLine, timed)

timer :: Effect Unit
timer =
  body $ Semigroupoid.do
    progressBar elapsedFraction
    p $ text progressLine
    RecordToVariant.do
      sliderLive @"Duration" # edits @"Duration changed"
      button @"Reset"
  # timed
