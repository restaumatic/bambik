module CounterShoelace (counterShoelace) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, projection, mvu, updated)
import PUI.Web.HTML (body, h4, text)
import PUI.Web.Shoelace (button, card)
import QualifiedDo.Semigroupoid as Semigroupoid

counterShoelace :: Effect Unit
counterShoelace =
  body $
    card { caption: "Counter" } $ ( Semigroupoid.do
        h4 (text @"count") # projection show # completed
        button @"increment" { label: "Count" } # updated (match { increment: const <<< increment })
    ) # mvu freshCount
