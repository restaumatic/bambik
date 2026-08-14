module CounterShoelace (counterShoelace) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, forField, mvu, updated)
import PUI.Web.HTML (body, h4, text)
import PUI.Web.Shoelace (button, card)
import QualifiedDo.Semigroupoid as Semigroupoid

counterShoelace :: Effect Unit
counterShoelace =
  body $
    card { caption: "Counter" } $ ( Semigroupoid.do
        h4 (text @"value") # forField @"count" show # completed
        button { label: "Count" } # updated (match { clicked: const <<< increment })
    ) # mvu freshCount
