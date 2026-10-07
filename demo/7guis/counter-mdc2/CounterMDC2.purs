module CounterMDC2 (counterMDC2) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countedLine, countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, button, headline4, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

counterMDC2 :: Effect Unit
counterMDC2 =
  body $
    Semigroupoid.do
      headline4 (text countLine) # shown
      button @"Count" {}
      snackbar @"Count" countedLine # fold increment
    # looped @( counted :: Int ) # with freshCount
