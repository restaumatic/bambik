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
-- |   * `request`               — Variant → Variant (+→+): a fake backend round-trip; both
-- |                               its accepted actions and its response cases are *deferred*
-- |                               (`forall v w`), pinned to a concrete contract at the use site
-- |   * `statusBar`/`eventLog`  — Variant → Record (+→×): render a response onto the page
module Showcase.Logic where

import Data.Profunctor.Row.Example (MyRowToRowProfunctor, button, checkbox, eventLog, request, statusBar, textInput)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Variant (Variant)
import QualifiedDo.Semigroupoid as Semigroupoid

-- | The checkout: fill the form, press `submit` or `cancel`, and the backend `request`
-- | resolves to a result page — `thankYou` (the placed order), `failure` (an error), or
-- | `cancelled`. `request` is deferred; the single annotation here is its **contract** —
-- | which actions it accepts and which responses it may return — which also pins the
-- | button merge and the response payloads:
-- |
-- | ```
-- |   form ──[submit | cancel]──▶ request ──▶ { thankYou | failure | cancelled } ──▶ page
-- |   ×→×          ×→+               +→+ (deferred)                                   +→×
-- | ```
checkout = Semigroupoid.do
  RecordToRecord.do      -- × → ×   the form: an input widget per field
    textInput @"email"
    textInput @"cardNumber"
    checkbox @"savePayment"
  RecordToVariant.do     -- × → +   the submit / cancel buttons, each firing the form
    button @"submit"
    button @"cancel"
  -- + → +   the backend round-trip. `request` is deferred; pinning it here to a concrete
  -- contract is what fixes which actions it takes and which responses it may return.
  ( request
      :: MyRowToRowProfunctor
           (Variant ( submit :: Record ( email :: String, cardNumber :: String, savePayment :: Boolean )
                    , cancel :: Record ( email :: String, cardNumber :: String, savePayment :: Boolean ) ))
           (Variant ( thankYou  :: Record ( email :: String, cardNumber :: String, savePayment :: Boolean )
                    , failure   :: String
                    , cancelled :: Record ( email :: String, cardNumber :: String, savePayment :: Boolean ) )) )
  VariantToRecord.do     -- + → ×   render the result page
    statusBar @"thankYou"
    eventLog @"failure"
    statusBar @"cancelled"
