-- | Diner's own components, over MDC3: what every station draws the same way.
module Diner.Design
  ( note
  , station
  , totalCell
  ) where

import Prelude (($), (>>>))

import PUI (Ocular, PUI)
import PUI.Web (Web, staticText, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (bodyLarge, dataCell, labelLarge)

-- | The pane a station draws in, set apart from the tab bar above it.
station :: Ocular (PUI Web)
station = div >>> "style" := "padding-top: 16px;"

-- | What a station shows where it has nothing to list.
note :: forall r. String -> PUI Web { | r } { | r }
note copy = bodyLarge $ staticText copy

totalCell :: forall r. ({ | r } -> String) -> PUI Web { | r } { | r }
totalCell copy = dataCell $ labelLarge $ text copy
