module CounterShoelace (counterShoelace) where

import Prelude ((#), ($), Unit)

import CounterLogic (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu)
import PUI.Web.HTML (h4, shown, text)
import PUI.Web.Shoelace (body, button, card)
import QualifiedDo.Semigroupoid as Semigroupoid

counterShoelace :: Effect Unit
counterShoelace =
  body $
    card $ ( Semigroupoid.do
      h4 (text countLine) # shown
      button @"Count" {} # applied increment
    ) # mvu freshCount
