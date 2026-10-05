module CounterMDC2 (counterMDC2) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, button, headline4)
import QualifiedDo.Semigroupoid as Semigroupoid

counterMDC2 :: Effect Unit
counterMDC2 =
  body $
    ( Semigroupoid.do
      headline4 (text countLine) # shown
      button @"Count" {}
      fold @"Count" increment
    ) # looped @( count :: Int ) # with freshCount
