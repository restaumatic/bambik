module CounterMDC2 (counterMDC2) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, prefixed, unshown, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, button, headline4)
import QualifiedDo.Semigroupoid as Semigroupoid

counterMDC2 :: Effect Unit
counterMDC2 =
  body $ Semigroupoid.do
    prefixed (headline4 (text countLine)) $ button @"Count"
    unshown @"Count" # fold increment
  # looped @( counted :: Int ) # with freshCount
