module CounterMDC2 (counterMDC2) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countedLine, countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Pipeline as Pipeline
import PUI.Web (text)
import PUI.Web.MDC2 (body, button, headline4, snackbar)

counterMDC2 :: Effect Unit
counterMDC2 =
  body $ Pipeline.do
    headline4 (text countLine)
    button @"Count" {}
    snackbar @"Count" countedLine # fold increment
  # looped @( counted :: Int ) # with freshCount
