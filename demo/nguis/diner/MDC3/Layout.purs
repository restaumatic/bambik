module Diner.MDC3.Layout
  ( blankCell
  , note
  , totalCell
  , totalLabelCell
  ) where

import Prelude (($))

import PUI (PUI)
import PUI.Web (Web, staticText, text)
import PUI.Web.HTML (strong)
import PUI.Web.MDC3 (bodyLarge, dataCell)

-- | What a station shows where it has nothing to list.
note :: forall r. String -> PUI Web { | r } { | r }
note copy = bodyLarge $ staticText copy

totalLabelCell :: forall r. PUI Web { | r } { | r }
totalLabelCell = dataCell $ strong $ staticText "Łącznie"

totalCell :: forall r. ({ | r } -> String) -> PUI Web { | r } { | r }
totalCell copy = dataCell $ strong $ text copy

blankCell :: forall r. PUI Web { | r } { | r }
blankCell = dataCell $ staticText ""
