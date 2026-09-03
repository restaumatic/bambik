module CounterFluent (counterFluent) where

import Prelude ((#), ($), Unit)

import CounterLogic (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu)
import PUI.Web.Fluent (button, card, title3)
import PUI.Web.HTML (body, shown, text)
import QualifiedDo.Category as Category

counterFluent :: Effect Unit
counterFluent =
  body $
    card $ ( Category.do
        title3 (text countLine) # shown
        button @"Count" {} # applied increment
    ) # mvu freshCount
