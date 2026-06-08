-- | **A checkout screen, built from row-profunctor UI widgets.**
-- |
-- | This reuses the widget leaves from `Data.Profunctor.Row.Example` — `textInput`,
-- | `checkbox`, `button`, `request`, `statusBar`, `eventLog` — and runs them at that
-- | module's concrete fake carrier `MyRowToRowProfunctor`. So the whole screen
-- | type-checks as a real composite, with no UI, effects, or hand-written optics.
-- |
-- | Each widget's *shape* is one of the four row-profunctor directions:
-- |
-- |   * `textInput`/`checkbox`  — Record → Record  (×→×): an editable field
-- |   * `button`                — Record → Variant (×→+): reads the form, fires an action
-- |   * `request`               — Variant → Variant (+→+): a fake backend round-trip whose
-- |                               response cases are *deferred* — one request resolves to
-- |                               `thankYou` *or* `failure`, inferred from the page below
-- |   * `statusBar`/`eventLog`  — Variant → Record (+→×): render a response onto the page
module Showcase.Logic where

import Data.Profunctor.Row.Example (MyRowToRowProfunctor, button, checkbox, eventLog, request, statusBar, textInput)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import QualifiedDo.Semigroupoid as Semigroupoid

-- | The checkout: fill the form, `submit` it, and the backend `request` resolves to a
-- | result page — `thankYou` (carrying the placed order) *or* `failure` (an error). The
-- | request's response cases are deferred; the page's two handlers are what fix them:
-- |
-- | ```
-- |   form ──submit──▶ request ──▶ { thankYou | failure } ──▶ page { thankYou, failure }
-- |   ×→×        ×→+      +→+ (deferred)                       +→×
-- | ```
-- |
-- | The signature pins the response payloads (the fake backend invents them); everything
-- | else is `@l` widgets, no inline annotations.
checkout
  :: MyRowToRowProfunctor
       (Record ( email :: String, cardNumber :: String, savePayment :: Boolean ))
       (Record ( thankYou :: Record ( email :: String, cardNumber :: String, savePayment :: Boolean )
               , failure :: String
               ))
checkout = Semigroupoid.do
  RecordToRecord.do      -- × → ×   the form: an input widget per field
    textInput @"email"
    textInput @"cardNumber"
    checkbox @"savePayment"
  button @"submit"       -- × → +   the submit button fires the whole form
  request @"submit"      -- + → +   backend round-trip: response is thankYou | failure (deferred)
  VariantToRecord.do     -- + → ×   render the result page
    statusBar @"thankYou"
    eventLog @"failure"
