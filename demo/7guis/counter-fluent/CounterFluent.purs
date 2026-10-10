module CounterFluent (counterFluent) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countedLine, countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web.Fluent (body, button, messageBar, title3)
import PUI.Web (shown, text)
import QualifiedDo.Semigroupoid as Semigroupoid

counterFluent :: Effect Unit
counterFluent =
  body $ Semigroupoid.do
    title3 (text countLine) # shown
    button @"Count" {}
    messageBar @"Count" countedLine # fold increment
  # looped @( counted :: Int ) # with freshCount
