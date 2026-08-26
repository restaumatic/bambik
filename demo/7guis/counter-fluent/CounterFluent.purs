module CounterFluent (counterFluent) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, updated, projection)
import PUI.Web.Fluent (button, card, title3)
import PUI.Web.HTML (body, shown, text)
import QualifiedDo.Semigroupoid as Pipeline

counterFluent :: Effect Unit
counterFluent =
  body $
    card $ ( Pipeline.do
        title3 (text @"count" # projection show) # shown
        button @"Count" {} # updated (match { "Count": const <<< increment })
    ) # mvu freshCount
