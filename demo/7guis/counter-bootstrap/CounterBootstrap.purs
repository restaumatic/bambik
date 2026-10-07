module CounterBootstrap (counterBootstrap) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countedLine, countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web.Bootstrap (body, button, toast)
import PUI.Web (shown, text)
import PUI.Web.HTML (h4)
import QualifiedDo.Semigroupoid as Semigroupoid

counterBootstrap :: Effect Unit
counterBootstrap =
  body $
    ( Semigroupoid.do
      h4 (text countLine) # shown
      button @"Count" {}
      toast @"Count" countedLine # fold increment
    ) # looped @( count :: Int ) # with freshCount
