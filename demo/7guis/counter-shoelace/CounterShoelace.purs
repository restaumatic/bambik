module CounterShoelace (counterShoelace) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, updated)
import PUI.Web.HTML (body, h4, shown)
import PUI.Web.Shoelace (button, card)
import QualifiedDo.Semigroupoid as Pipeline

counterShoelace :: Effect Unit
counterShoelace =
  body $
    card $ ( Pipeline.do
        h4 (shown @"count" show)
        button @"Count" {} # updated (match { "Count": const <<< increment })
    ) # mvu freshCount
