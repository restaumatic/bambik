module Timer.Fluent (timer) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Effect (Effect)
import PUI (edits)
import PUI.Web (text)
import PUI.Web.Fluent (body, body1, button, progressBar, slider)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, progressLine, timed)

timer :: Effect Unit
timer =
  body $ Semigroupoid.do
    progressBar elapsedFraction
    body1 $ text progressLine
    RecordToVariant.do
      slider @"Duration" # edits @"Duration changed"
      button @"Reset"
  # timed
