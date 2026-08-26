module CounterFluent (counterFluent) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, updated)
import PUI.Web.Fluent (button, card, title3)
import PUI.Web.HTML (body, shown)
import QualifiedDo.Semigroupoid as Pipeline

counterFluent :: Effect Unit
counterFluent =
  body $
    card $ ( Pipeline.do
        title3 (shown @"count" show)
        button @"Count" {} # updated (match { "Count": const <<< increment })
    ) # mvu freshCount
