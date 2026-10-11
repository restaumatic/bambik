module CounterHTML (counterHTML) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countedLine, countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Pipeline as Pipeline
import PUI.Web (text)
import PUI.Web.HTML (body, button, div, h4, output)

counterHTML :: Effect Unit
counterHTML =
  body $ div $ Pipeline.do
    h4 (text countLine)
    button @"Count" {}
    output @"Count" countedLine # fold increment
  # looped @( counted :: Int ) # with freshCount
