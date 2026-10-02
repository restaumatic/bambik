module CounterShoelace (counterShoelace) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu, state)
import PUI.Web (shown, text)
import PUI.Web.HTML (h4)
import PUI.Web.Shoelace (body, button)
import QualifiedDo.Semigroupoid as Semigroupoid

counterShoelace :: Effect Unit
counterShoelace =
  body $
    ( Semigroupoid.do
      state @"count" @Int
      h4 (text countLine) # shown
      button @"Count" {} # applied increment
    ) # mvu freshCount
