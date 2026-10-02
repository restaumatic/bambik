module CounterBootstrap (counterBootstrap) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu, state)
import PUI.Web.Bootstrap (body, button)
import PUI.Web (shown, text)
import PUI.Web.HTML (h4)
import QualifiedDo.Semigroupoid as Semigroupoid

counterBootstrap :: Effect Unit
counterBootstrap =
  body $
    ( Semigroupoid.do
      state @"count" @Int
      h4 (text countLine) # shown
      button @"Count" {} # applied increment
    ) # mvu freshCount
