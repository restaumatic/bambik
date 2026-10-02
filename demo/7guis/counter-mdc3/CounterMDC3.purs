module CounterMDC3 (counterMDC3) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu, state)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, button, headlineLarge)
import QualifiedDo.Semigroupoid as Semigroupoid

counterMDC3 :: Effect Unit
counterMDC3 =
  body $
    ( Semigroupoid.do
      state @"count" @Int
      headlineLarge (text countLine) # shown
      button @"Count" {} # applied increment
    ) # mvu freshCount
