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
-- |   * `request`/`modal`       — Variant → Variant (+→+): dispatch an action (`request` is
-- |                               a fake backend round-trip; `modal` is local)
-- |   * `statusBar`/`eventLog`  — Variant → Record (+→×): record the event as a field
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

-- | The whole checkout, flowing through all four merge directions — and now with no
-- | inline annotations: every widget is `@l`-parameterized, so each leaf names the
-- | single field/case it handles and the merges split unambiguously.
-- |
-- | ```
-- |   form ──▶ submit | cancel ──▶ submit | cancel ──▶ display
-- |   ×→×        ×→+                +→+                 +→×
-- | ```
checkout = Semigroupoid.do
  RecordToRecord.do      -- × → ×   the form: an input widget per field
    textInput @"email"
    textInput @"cardNumber"
    checkbox @"savePayment"
  RecordToVariant.do     -- × → +   actions: each button reads the form and fires its event
    button @"submit"
    button @"cancel"
  VariantToVariant.do    -- + → +   process each action: submit hits the backend, cancel is local
    request @"submit"
    modal @"cancel"
  VariantToRecord.do     -- + → ×   display each event
    statusBar @"submit"
    eventLog @"cancel"
