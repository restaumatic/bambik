module CounterFluent (counterFluent) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countedLine, countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Pipeline as Pipeline
import PUI.Web.Fluent (body, button, messageBar, title3)
import PUI.Web (text)

counterFluent :: Effect Unit
counterFluent =
  body $ Pipeline.do
    title3 (text countLine)
    button @"Count" {}
    messageBar @"Count" countedLine # fold increment
  # looped @( counted :: Int ) # with freshCount
