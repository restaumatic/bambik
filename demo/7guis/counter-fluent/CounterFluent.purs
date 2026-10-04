module CounterFluent (counterFluent) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu)
import PUI.Web.Fluent (body, button, title3)
import PUI.Web (shown, text)
import QualifiedDo.Semigroupoid as Semigroupoid

counterFluent :: Effect Unit
counterFluent =
  body $
    ( Semigroupoid.do
      title3 (text countLine) # shown
      button @"Count" {} # applied increment
    ) # mvu @( count :: Int ) freshCount
