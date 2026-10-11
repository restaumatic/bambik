module CounterMDC3 (counterMDC3) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countedLine, countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, prefixed, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, button, headlineLarge, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

counterMDC3 :: Effect Unit
counterMDC3 =
  body $ Semigroupoid.do
    prefixed (headlineLarge (text countLine)) $ button @"Count" {}
    snackbar @"Count" countedLine # fold increment
  # looped @( counted :: Int ) # with freshCount
