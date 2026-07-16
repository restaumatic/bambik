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
-- | of the code maps 1-1 to the order of the PUI, with no inline type
-- | annotations: MDC components are label-indexed row profunctors already
-- | (`MDC.filledTextField @"total"` edits one field, `MDC.button @"submit"`
-- | fires one event case, `MDC.snackbar # forCase @"orderSubmitted"` shows one
-- | message case), and every remaining row is closed either by a
-- | label-pinning helper (`field` for nesting sub-composites, `reading`)
-- | or by a model-function signature; inference propagates from the
-- | pipeline ends inward. Decoration is data or design-system config, not
-- | composition: the headline prefix rides in `reading`'s render function,
-- | card captions in `MDC.card`'s config.
-- |
-- | Variant editors (fulfillment, method) work through **record-shaped
-- | editor state**: the model keeps the variant (one case at a time), the
-- | editor keeps every payload. `dimap` brackets the variant in
-- | (`fulfillmentState` seeds absent payloads) and out (`fulfillmentCase`
-- | projects the selection); inside, a plain record merge of a `tabBar`
-- | selection component and `shownWhen` panes — wrapped in `looped`, the
-- | `×`-diagonal self-trace, so every emission is re-broadcast and the
-- | ensemble stays mutually consistent. Per-pane payload retention falls
-- | out of the merge gates (each gate holds its side's last contribution):
-- | switching a case away and back restores its state. A unit-payload
-- | variant (method) needs no panes and no loop — the bracket around one
-- | selection component suffices. The summary line is a `tapped` stage on
-- | the form's output, re-rendering on every emission.
-- |
-- | Merge-gate protocol: every record-merge operand must contain at least
-- | one element that echoes on `toUser` (text fields, `text` displays,
-- | selection components), so all gates open on the initial `loadOrder`
-- | render and the merged order flows to the buttons. Panes stay attached
-- | (`shownWhen` only hides them) — a detached editor cannot echo, which
-- | would starve the gates.
module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToRecord (asField, field, forField, tapped)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.RecordToVariant (asCase)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToRecord (forCase)
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.String (length)
import Data.Symbol (class IsSymbol)
import Data.Variant (case_, inj, match, on, prj) as Variant
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import PUI (PUI, action, debounced, looped, silence, with)
import PUI.HTML (attr, body, div, shownWhen, text) as HTML
import PUI.MDC (body1, button, card, elevation20, filledTextArea, filledTextField, headline6, indeterminateLinearProgress, segmentedButton, snackbar, tabBar) as MDC
import PUI.Web (Web)
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Semigroupoid
import Record (get)
import Type.Proxy (Proxy(..))

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
main =
  HTML.body $ ( MDC.elevation20 Semigroupoid.do
      MDC.indeterminateLinearProgress # action loadOrder
      RecordToRecord.do
        MDC.headline6 (HTML.text # lcmap ("Order " <> _) # lcmap _.value # forField @"shortId")
        MDC.card { caption: Just "Identifier" } $ RecordToRecord.do
          MDC.filledTextField { floatingLabel: "Short ID" } # asField @"shortId"
          MDC.filledTextField { floatingLabel: "Unique ID" } # asField @"orderId"
        MDC.card { caption: Just "Customer" }
          ( RecordToRecord.do
              MDC.filledTextField { floatingLabel: "First name" } # asField @"firstName"
              MDC.filledTextField { floatingLabel: "Last name" } # asField @"lastName"
          ) # field @"customer"
        -- the fulfillment variant is edited through record-shaped editor state:
        -- `dimap` brackets the variant in (seeding absent payloads) and out
        -- (projecting the selection), `looped` re-broadcasts every emission so
        -- the tab bar and panes stay mutually consistent, and the merge gates
        -- retain every pane's payload — switching away and back restores it
        MDC.card { caption: Just "Fulfillment" }
          ( ( RecordToRecord.do
                MDC.tabBar
                  [ { value: "dineIn", label: "Dine in", icon: Nothing }
                  , { value: "takeaway", label: "Takeaway", icon: Nothing }
                  , { value: "delivery", label: "Delivery", icon: Nothing }
                  ]
                  # asField @"selected"
                HTML.shownWhen (\r -> r.selected == "dineIn") (MDC.filledTextField { floatingLabel: "Table" } # asField @"table" # lcmap tableOf)
                HTML.shownWhen (\r -> r.selected == "takeaway") (MDC.filledTextField { floatingLabel: "Time" } # asField @"time" # lcmap timeOf)
                HTML.shownWhen (\r -> r.selected == "delivery")
                  ( ( RecordToRecord.do
                        MDC.filledTextField { floatingLabel: "Address" } # asField @"address"
                        MDC.body1 (HTML.text # lcmap (\address -> "Distance " <> distanceKm address <> " km") # lcmap _.value # forField @"address")
                    ) # lcmap addressOf
                  )
            ) # looped # dimap fulfillmentState fulfillmentCase
          ) # field @"fulfillment"
        MDC.card { caption: Just "Total" } $ MDC.filledTextField { floatingLabel: "Total" } # asField @"total"
        MDC.card { caption: Just "Payment" }
          ( RecordToRecord.do
              -- a unit-payload variant needs no panes and no loop — the bracket
              -- around a single selection component suffices (it echoes, so no
              -- `identity` echo wire either)
              MDC.segmentedButton
                [ { value: "cash", label: "Cash" }
                , { value: "card", label: "Card" }
                ]
                # asField @"selected" # dimap methodState methodCase # field @"method"
              MDC.filledTextField { floatingLabel: "Paid" } # asField @"paid"
              MDC.body1 (HTML.text # lcmap (\method -> "Paying by " <> methodText method) # lcmap _.value # forField @"method")
          ) # field @"payment"
        MDC.card { caption: Just "Remarks" } $ MDC.filledTextArea { columns: 80, rows: 3 } # asField @"remarks"
      -- a live view of the form's output: displays every emission and passes it
      -- on (a sibling inside the merge would update on load only)
      MDC.body1 (HTML.text # lcmap summarize) # debounced # tapped
      HTML.div >>> HTML.attr "style" "display: flex; gap: 8px;" $ RecordToVariant.do
        MDC.button { label: Just "Submit order", icon: Just "save" } # asCase @"submit"
        MDC.button { label: Just "Receipt", icon: Just "file" } # asCase @"printReceipt"
      VariantToVariant.do
        MDC.indeterminateLinearProgress # action (Variant.on (Proxy @"submit") submitOrder Variant.case_)
        MDC.indeterminateLinearProgress # action (Variant.on (Proxy @"printReceipt") printReceipt Variant.case_)
      VariantToRecord.do
        MDC.snackbar # forCase @"orderSubmitted"
        MDC.snackbar # forCase @"submissionFailed"
        MDC.snackbar # forCase @"receiptPrinted"
      silence
  ) # with unit

-- model functions

distanceKm :: String -> String
distanceKm address = show (length address)

methodText ::
  [ cash :: Unit
  , card :: Unit
  ]
  -> String
methodText = Variant.match
  { cash: const "cash"
  , card: const "card"
  }

-- editor state for the fulfillment ensemble: the model holds one case at a
-- time, the editor keeps every payload (retained by the merge gates while
-- the `looped` ensemble runs); `fulfillmentState`/`fulfillmentCase` bracket
-- the variant in (seeding absent payloads) and out (projecting the selection)
type FulfillmentState =
  { selected :: String
  , table :: String
  , time :: String
  , address :: String
  }

fulfillmentState ::
  [ dineIn :: { table :: String }
  , takeaway :: { time :: String }
  , delivery :: { address :: String }
  ]
  -> FulfillmentState
fulfillmentState = Variant.match
  { dineIn: \r -> { selected: "dineIn", table: r.table, time: "12:00", address: "" }
  , takeaway: \r -> { selected: "takeaway", table: "1", time: r.time, address: "" }
  , delivery: \r -> { selected: "delivery", table: "1", time: "12:00", address: r.address }
  }

fulfillmentCase :: FulfillmentState ->
  [ dineIn :: { table :: String }
  , takeaway :: { time :: String }
  , delivery :: { address :: String }
  ]
fulfillmentCase s =
  if s.selected == "dineIn" then .dineIn { table: s.table }
  else if s.selected == "takeaway" then .takeaway { time: s.time }
  else .delivery { address: s.address }

tableOf :: FulfillmentState -> { table :: String }
tableOf s = { table: s.table }

timeOf :: FulfillmentState -> { time :: String }
timeOf s = { time: s.time }

addressOf :: FulfillmentState -> { address :: String }
addressOf s = { address: s.address }

methodState ::
  [ cash :: Unit
  , card :: Unit
  ]
  -> { selected :: Maybe String }
methodState = Variant.match
  { cash: const { selected: Just "cash" }
  , card: const { selected: Just "card" }
  }

methodCase :: { selected :: String } ->
  [ cash :: Unit
  , card :: Unit
  ]
methodCase r = if r.selected == "cash" then .cash unit else .card unit

summarize :: Order -> String
summarize order =
  "Summary: Order " <> order.shortId
    <> " (uniquely " <> order.orderId <> ")"
    <> " for " <> order.customer.firstName <> " " <> order.customer.lastName
    <> ", fulfilled as " <> fulfillmentText order.fulfillment
    <> ", paid " <> order.payment.paid <> " by " <> methodText order.payment.method
  where
  fulfillmentText = Variant.match
    { dineIn: \r -> "dine in at table " <> r.table
    , takeaway: \r -> "takeaway at " <> r.time
    , delivery: \r -> "delivery to " <> r.address <> " (" <> distanceKm r.address <> " km away)"
    }

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




