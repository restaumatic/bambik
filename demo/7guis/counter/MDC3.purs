module Counter.MDC3 (counter) where

import Prelude ((#), ($), Unit)

import Counter (countLine, counted)
import Effect (Effect)
import PUI.Web (text)
import PUI.Web.MDC3 (body, button, headlineLarge)
import QualifiedDo.Semigroupoid as Semigroupoid

counter :: Effect Unit
counter =
  body $ Semigroupoid.do
    headlineLarge $ text countLine
    button @"Count"
  # counted
