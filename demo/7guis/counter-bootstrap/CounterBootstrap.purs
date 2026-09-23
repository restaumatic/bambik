module CounterBootstrap (counterBootstrap) where

import Prelude ((#), ($), Unit)

import CounterLogic (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu)
import PUI.Web.Bootstrap (body, button, card)
import PUI.Web.HTML (h4, shown, text)
import QualifiedDo.Semigroupoid as Semigroupoid

counterBootstrap :: Effect Unit
counterBootstrap =
  body $
    card $ ( Semigroupoid.do
      h4 (text countLine) # shown
      button @"Count" {} # applied increment
    ) # mvu freshCount
