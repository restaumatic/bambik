module Counter.Bootstrap (counter) where

import Prelude ((#), ($), Unit)

import Counter (countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (text)
import PUI.Web.Bootstrap (body, button)
import PUI.Web.HTML (h4)
import QualifiedDo.Semigroupoid as Semigroupoid

counter :: Effect Unit
counter =
  body $ Semigroupoid.do
    h4 $ text countLine
    button @"Count"
    fold @"Count" increment
  # looped # with freshCount
