module Counter.HTML (counter) where

import Prelude ((#), ($), Unit)

import Counter (countLine, counted)
import Effect (Effect)
import PUI.Web (text)
import PUI.Web.HTML (body, button, div, h4)
import QualifiedDo.Semigroupoid as Semigroupoid

counter :: Effect Unit
counter =
  body $ div $ Semigroupoid.do
    h4 $ text countLine
    button @"Count"
  # counted
