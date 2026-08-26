module CounterBootstrap (counterBootstrap) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, updated)
import PUI.Web.Bootstrap (button, card)
import PUI.Web.HTML (body, h4, shown)
import QualifiedDo.Semigroupoid as Pipeline

counterBootstrap :: Effect Unit
counterBootstrap =
  body $
    card $ ( Pipeline.do
        h4 (shown @"count" show)
        button @"Count" {} # updated (match { "Count": const <<< increment })
    ) # mvu freshCount
