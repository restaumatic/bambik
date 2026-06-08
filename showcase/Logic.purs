-- | **A checkout screen, built from row-profunctor UI widgets.**
-- |
-- | This reuses the widget leaves from `Data.Profunctor.Row.Example` — `textInput`,
-- | `checkbox`, `button`, `request`, `modal`, `statusBar`, `eventLog` — and runs them at
-- | that module's concrete fake carrier `MyRowToRowProfunctor`. So the whole screen
-- | type-checks as a real composite, with no UI, effects, or hand-written optics.
-- |
-- | Each widget's *shape* is one of the four row-profunctor directions:
-- |
-- |   * `textInput`/`checkbox`  — Record → Record  (×→×): an editable field
-- |   * `button`                — Record → Variant (×→+): reads the form, fires an action with it
-- |   * `actionButton`          — Record → Variant (×→+): fires an action carrying nothing (`Record ()`)
-- |   * `request`               — Variant → Variant (+→+): a fake backend round-trip; its
-- |                               response cases are *deferred* (`forall w`), pinned at use
-- |   * `modal`                 — Variant → Variant (+→+): a local handler (no backend)
-- |   * `statusBar`/`eventLog`  — Variant → Record (+→×): render a response onto the page
module Showcase.Logic where

import Data.Profunctor.Row.Example (MyRowToRowProfunctor, actionButton, button, checkbox, eventLog, modal, request, statusBar, textInput)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (Variant)
import QualifiedDo.Semigroupoid as Semigroupoid

-- | The checkout: fill the form, press `submit` or `cancel`. Only `submit` hits the
-- | backend `request` (deferred → `thankYou | failure`); `cancel` **bypasses it**, handled
-- | locally by `modal` → `cancelled`. Both responses land on the result page. Each
-- | handler is pinned to its contract (which also fixes the button merge and payloads):
-- |
-- | ```
-- |   form ──submit──▶ request ──▶ thankYou | failure  ┐
-- |   ×→×    ──cancel──▶ modal  ──▶ cancelled           ├──▶ page { thankYou, failure, cancelled }
-- |          ×→+         +→+                            ┘   +→×
-- | ```
checkout = Semigroupoid.do
  RecordToRecord.do      -- × → ×   the form: an input widget per field
    textInput @"email"
    textInput @"cardNumber"
    checkbox @"savePayment"
  RecordToVariant.do     -- × → +   submit fires the form; cancel fires nothing
    button @"submit"
    actionButton @"cancel"
  VariantToVariant.do    -- + → +   submit hits the backend; cancel bypasses it
    -- `request` (deferred) processes *only* submit — pinned here to its backend contract,
    -- a deferred response of thankYou | failure.
    ( request
        :: MyRowToRowProfunctor
             (Variant ( submit :: Record ( email :: String, cardNumber :: String, savePayment :: Boolean ) ))
             (Variant ( thankYou :: String, failure :: String )) )
    -- `cancel` never reaches the backend: a local `modal` turns it straight into `cancelled`.
    ( modal @"cancel" @"cancelled"
        :: MyRowToRowProfunctor
             (Variant ( cancel :: Record () ))
             (Variant ( cancelled :: String )) )
  VariantToRecord.do     -- + → ×   render the result page
    statusBar @"thankYou"
    eventLog @"failure"
    statusBar @"cancelled"
