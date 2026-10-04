module CounterHTML (counterHTML) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, mvu)
import PUI.Web (shown, text)
import PUI.Web.HTML (body, button, div, h4)
import QualifiedDo.Semigroupoid as Semigroupoid

counterHTML :: Effect Unit
counterHTML =
  body $ div $ ( Semigroupoid.do
    h4 (text countLine) # shown
    button @"Count" {}
    fold @"Count" increment
  ) # mvu @( count :: Int ) freshCount
