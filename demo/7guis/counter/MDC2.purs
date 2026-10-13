module Counter.MDC2 (counter) where

import Prelude ((#), ($), Unit)

import Counter (countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, unshown, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, button, headline4)
import QualifiedDo.Semigroupoid as Semigroupoid

counter :: Effect Unit
counter =
  body $ Semigroupoid.do
    headline4 (text countLine)
    button @"Count"
    unshown @"Count" # fold increment
  # looped @( counted :: Int ) # with freshCount
