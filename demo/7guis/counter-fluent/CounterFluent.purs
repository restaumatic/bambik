module CounterFluent (counterFluent) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, projection, mvu, updated)
import PUI.Web.Fluent (button, card, title3)
import PUI.Web.HTML (body, text)
import QualifiedDo.Semigroupoid as Semigroupoid

counterFluent :: Effect Unit
counterFluent =
  body $
    card $ ( Semigroupoid.do
        title3 (text @"count") # projection show # completed
        button @"Count" {} # updated (match { "Count": const <<< increment })
    ) # mvu freshCount
