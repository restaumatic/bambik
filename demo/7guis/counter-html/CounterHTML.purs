module CounterHTML (counterHTML) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countedLine, countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (shown, text)
import PUI.Web.HTML (body, button, div, h4, output)
import QualifiedDo.Semigroupoid as Semigroupoid

counterHTML :: Effect Unit
counterHTML =
  body $ div $ Semigroupoid.do
    h4 (text countLine) # shown
    button @"Count" {}
    output @"Count" countedLine # fold increment
  # looped @( counted :: Int ) # with freshCount
