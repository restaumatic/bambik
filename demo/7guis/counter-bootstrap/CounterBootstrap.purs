module CounterBootstrap (counterBootstrap) where

import Prelude ((#), ($), Unit, show)

import CounterLogic (freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu, projection)
import PUI.Web.Bootstrap (button, card)
import PUI.Web.HTML (body, h4, shown, text)
import QualifiedDo.Semigroupoid as Pipeline

counterBootstrap :: Effect Unit
counterBootstrap =
  body $
    card $ ( Pipeline.do
        h4 (text @"count" # projection show) # shown
        button @"Count" {} # applied increment
    ) # mvu freshCount
