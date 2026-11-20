-- | Diner's own components, over MDC3: what every station draws the same way.
module Diner.Design
  ( amountCell
  , amountHeader
  , controls
  , floor
  , heading
  , note
  , pagination
  , station
  , totalAmountCell
  , totalCell
  ) where

import Prelude ((#), ($), (>>>))

import PUI (Ocular, PUI)
import PUI.Web (Web, staticText, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (bodyLarge, columnHeader, dataCell, labelLarge, titleMedium)

-- | The pane a station draws in: a column no wider than a table reads
-- | well, its parts set apart from each other and from the tab bar above.
station :: Ocular (PUI Web)
station = div >>> "style" := "max-width: 960px; margin: 0 auto; padding-top: 16px; display: flex; flex-direction: column; gap: 12px;"

-- | The hall's tables side by side, as many to a row as fit.
floor :: Ocular (PUI Web)
floor = div >>> "style" := "display: grid; grid-template-columns: repeat(auto-fill, minmax(40px, 1fr)); gap: 4px;"

-- | A row's buttons side by side, whatever stands between them.
controls :: Ocular (PUI Web)
controls = div >>> "style" := "display: flex; align-items: center;"

-- | The row under a list that says where its page stands and turns it.
pagination :: Ocular (PUI Web)
pagination = div >>> "style" := "display: flex; align-items: center; justify-content: flex-end; gap: 4px;"

-- | What a station shows where it has nothing to list.
note :: forall r. String -> PUI Web { | r } { | r }
note copy = bodyLarge >>> "style" := "margin: 0;" $ staticText copy

-- | What a station is showing, over the part it heads.
heading :: forall r. ({ | r } -> String) -> PUI Web { | r } { | r }
heading copy = titleMedium >>> "style" := "margin: 0;" $ text copy

-- | A column of amounts: its heading and its cells set against the right
-- | edge, so the amounts line up by their grosze.
amountHeader :: forall r. String -> PUI Web { | r } { | r }
amountHeader title = columnHeader title # ("style" := "text-align: right;")

amountCell :: Ocular (PUI Web)
amountCell = dataCell >>> "style" := "text-align: right;"

-- | A total in the row that sums a column up, and one in a column of
-- | amounts.
totalCell :: forall r. ({ | r } -> String) -> PUI Web { | r } { | r }
totalCell copy = dataCell $ labelLarge $ text copy

totalAmountCell :: forall r. ({ | r } -> String) -> PUI Web { | r } { | r }
totalAmountCell copy = amountCell $ labelLarge $ text copy
