module Counter.Fluent (counter) where

import Prelude ((#), ($), Unit)

import Counter (countLine, counted)
import Effect (Effect)
import PUI.Web (text)
import PUI.Web.Fluent (body, button, title3)
import QualifiedDo.Semigroupoid as Semigroupoid

counter :: Effect Unit
counter =
  body $ Semigroupoid.do
    title3 $ text countLine
    button @"Count"
  # counted
