-- | Demo 1, ported to **row profunctors** — a single standalone module.
-- |
-- | The model is row-shaped and structural: the order is a `Record` row (all
-- | fields at once), every choice point an anonymous `Variant` row (one case
-- | at a time) — no ADTs, no nominal wrappers, no optics. The app is the
-- | four-direction pipeline over it:
-- |
-- |   load (action) → `×→×` order form → `×→+` event buttons →
-- |   `+→+` backend dispatch → `+→×` status snackbars
-- |
-- | with real MDC widgets as the merge operands, laid out so that the order
-- | of the code maps 1-1 to the order of the UI, with no inline type
-- | annotations: MDC components are label-indexed row profunctors already
-- | (`MDC.filledTextField @"total"` edits one field, `MDC.button @"submit"`
-- | fires one event case, `MDC.switch @"dineIn"` selects one variant case,
-- | `MDC.snackbar @"orderSubmitted"` shows one message case), and every
-- | remaining row is closed either by a label-pinning helper (`field` for
-- | nesting sub-composites, `reading`/`casePane`) or by a model-function
-- | signature; inference propagates from the pipeline ends inward. Decoration is data or design-system config, not
-- | composition: the headline prefix rides in `reading`'s render function,
-- | card captions in `MDC.card`'s config. Variant editors (fulfillment,
-- | method) are `synced` composites: input is broadcast to switches and
-- | panes, and every emission is cross-fed back into the siblings, so the
-- | view stays consistent; `MDC.switch` seeds and retains its case's last
-- | payload (`latch` inside), so switching away and back restores state.
-- |
-- | Merge-gate protocol: every record-merge operand must contain at least
-- | one element that echoes on `toUser` (text fields, `text` displays, or —
-- | for button-only editors — the `identity` wire inside `synced`), so all
-- | gates open on the initial `loadOrder` render and the merged order flows
-- | to the buttons.
-- |
-- | Known limitation (deliberate, pending design work): `recordToRecord`
-- | broadcasts downward and merges upward but does not cross-feed sibling
-- | outputs into sibling displays — the summary line and `reading` displays
-- | update only from upstream (i.e. on load), not on sibling edits. (Within
-- | a `synced` composite the cross-feed exists but skips the emitting
-- | member itself, so a pane's own `reading` is likewise load-only.)
module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, lcmap)
import Data.Profunctor.Row.RecordToRecord (field)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.String (length)
import Data.Symbol (class IsSymbol)
import Data.Variant (case_, inj, on, prj) as Variant
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import MDC as MDC
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Semigroupoid
import Record (get)
import Type.Proxy (Proxy(..))
import UI (UI, action, debounced, silence, synced)
import Web (Web, body, text, variant)

-- The one named type — the aggregate the whole pipeline revolves around.
-- Everything inside it is structural: anonymous record and variant rows.
type Order =
  { shortId :: String
  , orderId :: String
  , customer ::
      { firstName :: String
      , lastName :: String
      }
  , fulfillment ::
      [ dineIn :: { table :: String }
      , takeaway :: { time :: String }
      , delivery :: { address :: String }
      ]
  , total :: String
  , payment ::
      { method ::
          [ cash :: Unit
          , card :: Unit
          ]
      , paid :: String
      }
  , remarks :: String
  }

main :: Effect Unit
main = body @Unit $ MDC.elevation20 Semigroupoid.do
  action loadOrder MDC.indeterminateLinearProgress
  RecordToRecord.do
    MDC.headline6 $ reading @"shortId" ("Order " <> _)
    MDC.card { caption: Just "Identifier" } $ RecordToRecord.do
      MDC.filledTextField @"shortId" { floatingLabel: "Short ID" }
      MDC.filledTextField @"orderId" { floatingLabel: "Unique ID" }
    field @"customer" $ MDC.card { caption: Just "Customer" } $ RecordToRecord.do
      MDC.filledTextField @"firstName" { floatingLabel: "First name" }
      MDC.filledTextField @"lastName" { floatingLabel: "Last name" }
    field @"fulfillment" $ MDC.card { caption: Just "Fulfillment" } $ synced
      [ MDC.switch @"dineIn" { label: Just "Dine in", icon: Nothing } { table: "1" }
      , MDC.switch @"takeaway" { label: Just "Takeaway", icon: Nothing } { time: "12:00" }
      , MDC.switch @"delivery" { label: Just "Delivery", icon: Nothing } { address: "" }
      , casePane @"dineIn" $ MDC.filledTextField @"table" { floatingLabel: "Table" }
      , casePane @"takeaway" $ MDC.filledTextField @"time" { floatingLabel: "Time" }
      , casePane @"delivery" $ RecordToRecord.do
          MDC.filledTextField @"address" { floatingLabel: "Address" }
          MDC.body1 $ reading @"address" \address -> "Distance " <> distanceKm address <> " km"
      ]
    MDC.card { caption: Just "Total" } $ MDC.filledTextField @"total" { floatingLabel: "Total" }
    field @"payment" $ MDC.card { caption: Just "Payment" } $ RecordToRecord.do
      -- `identity` is the echo wire: buttons don't echo on render, so a
      -- button-only editor needs this pass-through member to open the
      -- record-merge gate (every operand must echo what it knows)
      field @"method" $ synced
        [ identity
        , MDC.switch @"cash" { label: Just "Cash", icon: Nothing } unit
        , MDC.switch @"card" { label: Just "Card", icon: Nothing } unit
        ]
      MDC.filledTextField @"paid" { floatingLabel: "Paid" }
      MDC.body1 $ reading @"method" \method -> "Paying by " <> methodText method
    MDC.card { caption: Just "Remarks" } $ MDC.filledTextArea @"remarks" { columns: 80, rows: 3 }
    debounced $ MDC.body1 $ lcmap summarize text
  RecordToVariant.do
    MDC.button @"submit" { label: Just "Submit order", icon: Just "save" }
    MDC.button @"printReceipt" { label: Just "Receipt", icon: Just "file" }
  VariantToVariant.do
    action (Variant.on (Proxy @"submit") submitOrder Variant.case_) MDC.indeterminateLinearProgress
    action (Variant.on (Proxy @"printReceipt") printReceipt Variant.case_) MDC.indeterminateLinearProgress
  VariantToRecord.do
    MDC.snackbar @"orderSubmitted"
    MDC.snackbar @"submissionFailed"
    MDC.snackbar @"receiptPrinted"
  silence

-- model functions

distanceKm :: String -> String
distanceKm address = show (length address)

methodText ::
  [ cash :: Unit
  , card :: Unit
  ]
  -> String
methodText = Variant.case_
  # Variant.on (Proxy @"cash") (const "cash")
  # Variant.on (Proxy @"card") (const "card")

summarize :: Order -> String
summarize order =
  "Summary: Order " <> order.shortId
    <> " (uniquely " <> order.orderId <> ")"
    <> " for " <> order.customer.firstName <> " " <> order.customer.lastName
    <> ", fulfilled as " <> fulfillmentText order.fulfillment
    <> ", paid " <> order.payment.paid <> " by " <> methodText order.payment.method
  where
  fulfillmentText = Variant.case_
    # Variant.on (Proxy @"dineIn") (\r -> "dine in at table " <> r.table)
    # Variant.on (Proxy @"takeaway") (\r -> "takeaway at " <> r.time)
    # Variant.on (Proxy @"delivery") (\r -> "delivery to " <> r.address <> " (" <> distanceKm r.address <> " km away)")

-- asynchronous actions

loadOrder :: Unit -> Aff Order
loadOrder _ = do
  liftEffect $ log "loading order"
  delay (Milliseconds 1000.0)
  liftEffect $ log "loaded order"
  pure
    { shortId: "7"
    , orderId: "4617821"
    , customer:
        { firstName: "John"
        , lastName: "Doe"
        }
    , fulfillment: .takeaway { time: "8:30" }
    , total: "12.30"
    , payment:
        { method: .cash unit
        , paid: "0.00"
        }
    , remarks: "Very spicy, please!"
    }

submitOrder :: Order -> Aff
  [ orderSubmitted :: String
  , submissionFailed :: String
  ]
submitOrder order = do
  liftEffect $ log $ "submitting order " <> order.orderId
  delay (Milliseconds 1000.0)
  if order.total == ""
    then do
      liftEffect $ log "order submission failed"
      pure $ .submissionFailed ("Order " <> order.shortId <> " rejected: missing total")
    else do
      liftEffect $ log "submitted order"
      pure $ .orderSubmitted ("Order " <> order.shortId <> " submitted")

printReceipt :: Order -> Aff [ receiptPrinted :: String ]
printReceipt order = do
  liftEffect $ log $ "printing receipt for order " <> order.orderId
  delay (Milliseconds 2000.0)
  liftEffect $ log $ "printed receipt for order " <> order.orderId
  pure $ .receiptPrinted ("Receipt for order " <> order.shortId <> " printed")

-- row-generic helpers (candidates for the library once proven here)

-- | A single-field display as a record-merge operand: reads one field,
-- | contributes nothing.
reading :: forall @l a r. IsSymbol l => Cons l a () r => (a -> String) -> UI Web { | r } {}
reading render = lcmap (\r -> render (get (Proxy @l) r)) text


-- | A case *pane*: the sub-form for one case, attached to the DOM only while
-- | that case is selected (`Web.variant` hides on the other cases), emitting
-- | back into the same case.
casePane :: forall @l f b s. IsSymbol l => Cons l f b s => UI Web f f -> UI Web [ | s ] [ | s ]
casePane w = dimap (Variant.prj (Proxy @l)) (Variant.inj (Proxy @l)) (variant w)


