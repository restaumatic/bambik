-- | **A checkout screen, built from row-profunctor UI widgets.**
-- |
-- | This reuses the widget leaves from `Data.Profunctor.Row.Example` — `textInput`,
-- | `checkbox`, `button`, `notification`, `modal`, `statusBar`, `eventLog` — and runs
-- | them at that module's concrete fake carrier `MyRowToRowProfunctor`. So the whole
-- | screen type-checks as a real composite, with no UI, effects, or hand-written optics:
-- | the widgets compose by row-profunctor **merge** (the four `*.do` blocks, one per
-- | direction) and by `Semigroupoid.do` **flow** (form → actions → outcomes → display).
-- |
-- | Each widget's *shape* is one of the four row-profunctor directions:
-- |
-- |   * `textInput`/`checkbox`  — Record → Record  (×→×): an editable field
-- |   * `button`                — Record → Variant (×→+): reads the form, fires an action
-- |   * `notification`/`modal`  — Variant → Variant (+→+): turn an action into an outcome
-- |   * `statusBar`/`eventLog`  — Variant → Record (+→×): display the outcome
module Showcase.Logic where

import Data.Profunctor.Row.Example (MyRowToRowProfunctor, button, checkbox, eventLog, modal, notification, statusBar, textInput)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (Variant)
import QualifiedDo.Semigroupoid as Semigroupoid

-- | The whole checkout, flowing through all four merge directions:
-- |
-- | ```
-- |   form ──▶ submit | cancel ──▶ placed | aborted ──▶ display
-- |   ×→×        ×→+                +→+                  +→×
-- | ```
-- |
-- | The `Variant → Variant` / `Variant → Record` widgets are fully polymorphic, so each
-- | leaf carries an annotation pinning which case it handles — which also pins the
-- | upstream `button` outputs.
checkout
  :: MyRowToRowProfunctor
       (Record ( email :: String, cardNumber :: String, savePayment :: Boolean ))
       (Record ())
checkout = Semigroupoid.do
  RecordToRecord.do      -- × → ×   the form: an input widget per field
    textInput @"email"
    textInput @"cardNumber"
    checkbox @"savePayment"
  RecordToVariant.do     -- × → +   actions: each button reads the form and fires its event
    button @"submit"
    button @"cancel"
  VariantToVariant.do    -- + → +   process: turn each action into an outcome
    ( notification
        :: MyRowToRowProfunctor
             (Variant ( submit :: Record ( email :: String, cardNumber :: String, savePayment :: Boolean ) ))
             (Variant ( placed :: String )) )
    ( modal
        :: MyRowToRowProfunctor
             (Variant ( cancel :: Record ( email :: String, cardNumber :: String, savePayment :: Boolean ) ))
             (Variant ( aborted :: String )) )
  VariantToRecord.do     -- + → ×   display: render each outcome
    (statusBar :: MyRowToRowProfunctor (Variant ( placed :: String )) (Record ()))
    (eventLog  :: MyRowToRowProfunctor (Variant ( aborted :: String )) (Record ()))
