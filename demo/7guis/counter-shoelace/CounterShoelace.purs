module CounterShoelace (counterShoelace) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countedLine, countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Pipeline as Pipeline
import PUI.Web (text)
import PUI.Web.HTML (h4)
import PUI.Web.Shoelace (body, button, toast)

counterShoelace :: Effect Unit
counterShoelace =
  body $ Pipeline.do
    h4 (text countLine)
    button @"Count" {}
    toast @"Count" countedLine # fold increment
  # looped @( counted :: Int ) # with freshCount
