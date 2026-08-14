module CounterBootstrap (counterBootstrap) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, forField, mvu, updated)
import PUI.Web.Bootstrap (button, card)
import PUI.Web.HTML (body, h4, text)
import QualifiedDo.Semigroupoid as Semigroupoid

counterBootstrap :: Effect Unit
counterBootstrap =
  body $
    card { caption: "Counter" } $ ( Semigroupoid.do
        h4 (text @"value") # forField @"count" show # completed
        button { label: "Count" } # updated (match { clicked: const <<< increment })
    ) # mvu freshCount
