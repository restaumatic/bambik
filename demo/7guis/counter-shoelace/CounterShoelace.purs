module CounterShoelace (counterShoelace) where

import Prelude ((#), ($), Unit, show)

import CounterLogic (freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu, projection)
import PUI.Web.HTML (body, h4, shown, text)
import PUI.Web.Shoelace (button, card)
import QualifiedDo.Semigroupoid as Pipeline

counterShoelace :: Effect Unit
counterShoelace =
  body $
    card $ ( Pipeline.do
        h4 (text @"count" # projection show) # shown
        button @"Count" {} # applied increment
    ) # mvu freshCount
