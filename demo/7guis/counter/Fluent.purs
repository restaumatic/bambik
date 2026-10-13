module Counter.Fluent (counter) where

import Prelude ((#), ($), Unit)

import Counter (countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, unshown, with)
import PUI.Web.Fluent (body, button, title3)
import PUI.Web (text)
import QualifiedDo.Semigroupoid as Semigroupoid

counter :: Effect Unit
counter =
  body $ Semigroupoid.do
    title3 (text countLine)
    button @"Count"
    unshown @"Count" # fold increment
  # looped @( counted :: Int ) # with freshCount
