module CounterBootstrap (counterBootstrap) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countedLine, countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, prefixed, with)
import PUI.Web.Bootstrap (body, button, toast)
import PUI.Web (text)
import PUI.Web.HTML (h4)
import QualifiedDo.Semigroupoid as Semigroupoid

counterBootstrap :: Effect Unit
counterBootstrap =
  body $ Semigroupoid.do
    prefixed (h4 (text countLine)) $ button @"Count" {}
    toast @"Count" countedLine # fold increment
  # looped @( counted :: Int ) # with freshCount
