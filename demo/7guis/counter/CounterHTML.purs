module CounterHTML (counterHTML) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, unshown, with)
import PUI.Web (text)
import PUI.Web.HTML (body, button, div, h4)
import QualifiedDo.Semigroupoid as Semigroupoid

counterHTML :: Effect Unit
counterHTML =
  body $ div $ Semigroupoid.do
    h4 (text countLine)
    button @"Count"
    unshown @"Count" # fold increment
  # looped @( counted :: Int ) # with freshCount
