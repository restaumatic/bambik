module CounterFluent (counterFluent) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, unshown, with)
import PUI.Web.Fluent (body, button, title3)
import PUI.Web (text)
import QualifiedDo.Semigroupoid as Semigroupoid

counterFluent :: Effect Unit
counterFluent =
  body $ Semigroupoid.do
    title3 (text countLine)
    button @"Count"
    unshown @"Count" # fold increment
  # looped @( counted :: Int ) # with freshCount
