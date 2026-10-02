module CounterHTML (counterHTML) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu, state)
import PUI.Web (shown, text)
import PUI.Web.HTML (body, button, div, h4)
import QualifiedDo.Semigroupoid as Semigroupoid

counterHTML :: Effect Unit
counterHTML =
  body $ div $ ( Semigroupoid.do
    state @"count" @Int
    h4 (text countLine) # shown
    button @"Count" {} # applied increment
  ) # mvu freshCount
