module CounterFluent (counterFluent) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, forField, mvu, updated)
import PUI.Web.Fluent (button, card, title3)
import PUI.Web.HTML (body, text)
import QualifiedDo.Semigroupoid as Semigroupoid

counterFluent :: Effect Unit
counterFluent =
  body $
    card { caption: "Counter" } $ ( Semigroupoid.do
        title3 text # forField @"count" show # completed
        button { label: "Count" } # updated (match { clicked: const <<< increment })
    ) # mvu freshCount
