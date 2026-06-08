-- | **A checkout screen, built from row-profunctor UI widgets.**
-- |
-- | This reuses the widget leaves from `Data.Profunctor.Row.Example` — `textInput`,
-- | `checkbox`, `button`, `request`, `modal`, `statusBar`, `eventLog` — and runs
-- | them at that module's concrete fake carrier `MyRowToRowProfunctor`. So the whole
-- | screen type-checks as a real composite, with no UI, effects, or hand-written optics:
-- | the widgets compose by row-profunctor **merge** (the four `*.do` blocks, one per
-- | direction) and by `Semigroupoid.do` **flow** (form → actions → outcomes → display).
-- |
-- | Each widget's *shape* is one of the four row-profunctor directions:
-- |
-- |   * `textInput`/`checkbox`  — Record → Record  (×→×): an editable field
-- |   * `button`                — Record → Variant (×→+): reads the form, fires an action
-- |   * `request`/`modal`       — Variant → Variant (+→+): turn an action into an outcome
-- |                               (`request` is a fake backend round-trip; `modal` is local)
-- |   * `statusBar`/`eventLog`  — Variant → Record (+→×): render an outcome onto the page
-- |
-- | Every widget is `@l`-parameterized (pins the single field/case it handles), so the
-- | merges split unambiguously and the body needs no type annotations at all.
module Showcase.Logic where

import Data.Profunctor.Row.Example (button, checkbox, eventLog, modal, request, statusBar, textInput)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import QualifiedDo.Semigroupoid as Semigroupoid

-- | The whole checkout, flowing through all four merge directions to a result page —
-- | with no inline annotations (every widget is `@l`-parameterized):
-- |
-- | ```
-- |   form ──▶ submit | cancel ──▶ thankYou | cancelled ──▶ page { thankYou, cancelled }
-- |   ×→×        ×→+                +→+                       +→×
-- | ```
-- |
-- | so it resolves to `… (Record ( email, cardNumber, savePayment ))
-- |                       (Record ( thankYou :: Record …, cancelled :: Record … ))` —
-- | a checkout status / thank-you page carrying the placed (or cancelled) order.
checkout = Semigroupoid.do
  RecordToRecord.do      -- × → ×   the form: an input widget per field
    textInput @"email"
    textInput @"cardNumber"
    checkbox @"savePayment"
  RecordToVariant.do     -- × → +   actions: each button reads the form and fires its event
    button @"submit"
    button @"cancel"
  VariantToVariant.do    -- + → +   submit → backend → thankYou; cancel → cancelled
    request @"submit" @"thankYou"
    modal @"cancel" @"cancelled"
  VariantToRecord.do     -- + → ×   render the checkout result page
    statusBar @"thankYou"
    eventLog @"cancelled"
