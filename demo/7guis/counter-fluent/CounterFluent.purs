module CounterFluent (counterFluent) where

import Prelude ((#), ($), Unit, show)

import CounterLogic (freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu, projection)
import PUI.Web.Fluent (button, card, title3)
import PUI.Web.HTML (body, shown, text)
import QualifiedDo.Semigroupoid as Pipeline

counterFluent :: Effect Unit
counterFluent =
  body $
    card $ ( Pipeline.do
        title3 (text @"count" # projection show) # shown
        button @"Count" {} # applied increment
    ) # mvu freshCount
