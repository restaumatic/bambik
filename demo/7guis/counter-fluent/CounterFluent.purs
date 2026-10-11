module CounterFluent (counterFluent) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countedLine, countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, prefixed, with)
import PUI.Web.Fluent (body, button, messageBar, title3)
import PUI.Web (text)
import QualifiedDo.Semigroupoid as Semigroupoid

counterFluent :: Effect Unit
counterFluent =
  body $ Semigroupoid.do
    prefixed (title3 (text countLine)) $ button @"Count" {}
    messageBar @"Count" countedLine # fold increment
  # looped @( counted :: Int ) # with freshCount
