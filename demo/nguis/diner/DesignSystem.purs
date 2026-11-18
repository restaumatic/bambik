-- | Diner's own components, over MDC3: what every station draws the same way.
module Diner.DesignSystem
  ( note
  , totalCell
  ) where

import Prelude (($))

import PUI (PUI)
import PUI.Web (Web, staticText, text)
import PUI.Web.HTML (strong)
import PUI.Web.MDC3 (bodyLarge, dataCell)

-- | What a station shows where it has nothing to list.
note :: forall r. String -> PUI Web { | r } { | r }
note copy = bodyLarge $ staticText copy

totalCell :: forall r. ({ | r } -> String) -> PUI Web { | r } { | r }
totalCell copy = dataCell $ strong $ text copy
