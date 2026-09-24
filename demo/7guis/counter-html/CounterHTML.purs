module CounterHTML (counterHTML) where

import Prelude ((#), ($), Unit)

import CounterLogic (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu)
import PUI.Web (shown, staticText, text)
import PUI.Web.HTML (body, button, div, h4)
import QualifiedDo.Semigroupoid as Semigroupoid

counterHTML :: Effect Unit
counterHTML =
  body $ div $ ( Semigroupoid.do
    h4 (text countLine) # shown
    button @"Count" (staticText "Count") # applied increment
  ) # mvu freshCount
