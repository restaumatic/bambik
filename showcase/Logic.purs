-- | **A checkout screen, built from row-profunctor UI widgets.**
-- |
-- | This reuses the widget leaves from `Data.Profunctor.Row.Example` — `textInput`,
-- | `checkbox`, `button` — and runs them at that module's concrete fake carrier
-- | `MyRowToRowProfunctor`. So the whole screen type-checks as a real composite, with no
-- | UI, effects, or hand-written optics: the widgets compose by row-profunctor **merge**
-- | (`RecordToRecord.do` for the form, `RecordToVariant.do` for the actions) and by
-- | `Semigroupoid.do` **flow** (the form feeds the action buttons).
-- |
-- | Each widget's *shape* is one of the row-profunctor directions:
-- |
-- |   * `textInput`/`checkbox` — Record → Record  (×→×): an editable field
-- |   * `button`               — Record → Variant (×→+): reads the form, fires an action
module Showcase.Logic where

import Data.Profunctor.Row.Example (MyRowToRowProfunctor, button, checkbox, textInput)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (Variant)
import QualifiedDo.Semigroupoid as Semigroupoid

-- | The checkout screen: a **form** of input widgets that flows into two action
-- | **buttons**, each firing the completed form as a `submit` / `cancel` event.
-- |
-- | ```
-- |   form ─────────────────▶ action event
-- |   RecordToRecord.do       RecordToVariant.do  (submit | cancel)
-- | ```
checkout
  :: MyRowToRowProfunctor
       (Record ( email :: String, cardNumber :: String, savePayment :: Boolean ))
       (Variant ( submit :: Record ( email :: String, cardNumber :: String, savePayment :: Boolean )
                , cancel :: Record ( email :: String, cardNumber :: String, savePayment :: Boolean )
                ))
checkout = Semigroupoid.do
  RecordToRecord.do      -- the form: an input widget per field
    textInput @"email"
    textInput @"cardNumber"
    checkbox @"savePayment"
  RecordToVariant.do     -- actions: each button reads the form and fires its event
    button @"submit"
    button @"cancel"
