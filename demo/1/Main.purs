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
-- | annotations: every row is closed either by a label-pinning helper
-- | (`field`/`reading`/`switch`/`casePane`/`event`/`statusLine`) or by a
-- | model-function signature, and inference propagates from the pipeline
-- | ends inward. Sibling decoration (captions, static text) rides along via
-- | `fold`/`<>` (UI's broadcast `Semigroup` — decorations never emit, so
-- | they don't gate the merges).
-- |
-- | Merge-gate protocol: every record-merge operand must contain at least
-- | one element that echoes on `toUser` (text fields, `text` displays, or —
-- | for button-only editors — the `identity` wire), so all gates open on the
-- | initial `loadOrder` render and the merged order flows to the buttons.
-- |
-- | Known limitations (deliberate, pending design work):
-- |   * No intra-form reactivity: `recordToRecord` broadcasts downward and
-- |     merges upward but does not cross-feed sibling outputs into sibling
-- |     displays (the pre-row demo did that with a dedicated mirroring
-- |     combinator, `pendo`, since dissolved). Case panes, the summary line,
-- |     and the `reading` displays update only from upstream (i.e. on load),
-- |     not on sibling edits.
-- |   * Case switches emit fixed payloads: switching a case away and back
-- |     resets its sub-form to the seed value. Retaining the last payload
-- |     per case is a `Retaining`/`Reel` follow-up.
module Main (main) where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord (property)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (recordToCase)
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
import QualifiedDo.Semigroupoid as Flow
import Record (get)
import Type.Proxy (Proxy(..))
import UI (UI, action, debounced)
import Web (Web, body, staticText, text, variant)

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
main = body @Unit $ MDC.elevation20 Flow.do
  action loadOrder MDC.indeterminateLinearProgress
  RecordToRecord.do
    MDC.headline6 $ fold
      [ staticText "Order "
      , reading @"shortId" identity
      ]
    MDC.card $ fold
      [ MDC.caption $ staticText "Identifier"
      , RecordToRecord.do
          field @"shortId" $ MDC.filledTextField { floatingLabel: "Short ID" }
          field @"orderId" $ MDC.filledTextField { floatingLabel: "Unique ID" }
      ]
    field @"customer" $ MDC.card $ fold
      [ MDC.caption $ staticText "Customer"
      , RecordToRecord.do
          field @"firstName" $ MDC.filledTextField { floatingLabel: "First name" }
          field @"lastName" $ MDC.filledTextField { floatingLabel: "Last name" }
      ]
    field @"fulfillment" $ MDC.card $ fold
      [ MDC.caption $ staticText "Fulfillment"
      -- switches emit fixed seed payloads (see header: known limitations)
      , switch @"dineIn" "Dine in" { table: "1" }
      , switch @"takeaway" "Takeaway" { time: "12:00" }
      , switch @"delivery" "Delivery" { address: "" }
      , casePane @"dineIn" $ field @"table" $ MDC.filledTextField { floatingLabel: "Table" }
      , casePane @"takeaway" $ field @"time" $ MDC.filledTextField { floatingLabel: "Time" }
      , casePane @"delivery" $ RecordToRecord.do
          field @"address" $ MDC.filledTextField { floatingLabel: "Address" }
          MDC.body1 $ reading @"address" \address -> "Distance " <> distanceKm address <> " km"
      ]
    MDC.card $ fold
      [ MDC.caption $ staticText "Total"
      , field @"total" $ MDC.filledTextField { floatingLabel: "Total" }
      ]
    field @"payment" $ MDC.card $ fold
      [ MDC.caption $ staticText "Payment"
      , RecordToRecord.do
          field @"method" $ fold
            -- `identity` is the echo wire: buttons don't echo on render, so a
            -- button-only editor needs this pass-through contribution to open
            -- the record-merge gate (every operand must echo what it knows)
            [ identity
            , switch @"cash" "Cash" unit
            , switch @"card" "Card" unit
            ]
          field @"paid" $ MDC.filledTextField { floatingLabel: "Paid" }
          MDC.body1 $ reading @"method" \method -> "Paying by " <> methodText method
      ]
    MDC.card $ fold
      [ MDC.caption $ staticText "Remarks"
      , field @"remarks" $ MDC.filledTextArea { columns: 80, rows: 3 }
      ]
    debounced $ MDC.body1 $ lcmap summarize text
  RecordToVariant.do
    event @"submit" $ MDC.containedButton { label: Just "Submit order", icon: Just "save" }
    event @"printReceipt" $ MDC.containedButton { label: Just "Receipt", icon: Just "file" }
  VariantToVariant.do
    action (Variant.on (Proxy @"submit") submitOrder Variant.case_) MDC.indeterminateLinearProgress
    action (Variant.on (Proxy @"printReceipt") printReceipt Variant.case_) MDC.indeterminateLinearProgress
  VariantToRecord.do
    statusLine @"orderSubmitted"
    statusLine @"submissionFailed"
    statusLine @"receiptPrinted"
  staticText ""

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

-- | A single-field editor as a record-merge operand: the singleton-shot
-- | `property` (background pinned empty).
field :: forall @l a r. IsSymbol l => Cons l a () r => UI Web a a -> UI Web { | r } { | r }
field = property @l

-- | A single-field display as a record-merge operand: reads one field,
-- | contributes nothing.
reading :: forall @l a r. IsSymbol l => Cons l a () r => (a -> String) -> UI Web { | r } {}
reading render = lcmap (\r -> render (get (Proxy @l) r)) text

-- | A case *switch*: a button emitting case `l` with the given payload into
-- | the variant under edit (enabled once the current value has arrived).
switch :: forall @l f b s. IsSymbol l => Cons l f b s => String -> f -> UI Web [ | s ] [ | s ]
switch label payload = rmap (const (Variant.inj (Proxy @l) payload)) (MDC.containedButton { label: Just label, icon: Nothing })

-- | A case *pane*: the sub-form for one case, attached to the DOM only while
-- | that case is selected (`Web.variant` hides on the other cases), emitting
-- | back into the same case.
casePane :: forall @l f b s. IsSymbol l => Cons l f b s => UI Web f f -> UI Web [ | s ] [ | s ]
casePane w = dimap (Variant.prj (Proxy @l)) (Variant.inj (Proxy @l)) (variant w)

-- | An event button as a record-to-variant operand: reads the whole record
-- | and fires it as event case `l` on click.
event :: forall @l r s. IsSymbol l => Cons l { | r } () s => UI Web { | r } { | r } -> UI Web { | r } [ | s ]
event = recordToCase @l

-- | A status snackbar for one message case.
statusLine :: forall @l r. IsSymbol l => Cons l String () r => UI Web [ | r ] {}
statusLine = MDC.snackbar $ lcmap (Variant.on (Proxy @l) identity Variant.case_) text
