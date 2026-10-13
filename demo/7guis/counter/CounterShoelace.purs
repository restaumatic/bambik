module CounterShoelace (counterShoelace) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, prefixed, unshown, with)
import PUI.Web (text)
import PUI.Web.HTML (h4)
import PUI.Web.Shoelace (body, button)
import QualifiedDo.Semigroupoid as Semigroupoid

counterShoelace :: Effect Unit
counterShoelace =
  body $ Semigroupoid.do
    prefixed (h4 (text countLine)) $ button @"Count"
    unshown @"Count" # fold increment
  # looped @( counted :: Int ) # with freshCount
