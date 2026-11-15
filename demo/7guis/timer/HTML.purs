module Timer.HTML (timer) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Effect (Effect)
import PUI (edits)
import PUI.Web (staticText, text)
import PUI.Web.HTML (body, button, div, label, p, progress, rangeInput)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, progressLine, timed)

timer :: Effect Unit
timer =
  body $ div $ Semigroupoid.do
    progress elapsedFraction
    p $ text progressLine
    RecordToVariant.do
      ( p $ label $ Semigroupoid.do
        staticText "Duration "
        rangeInput @"Duration" ) # edits @"Duration changed"
      button @"Reset"
  # timed
