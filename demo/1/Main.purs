module Main (main) where

import Prelude ((#), ($), (<>), (==), Unit, const, discard, pure, show, unit)

import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.String (length)
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import PUI (action, asCase, asField, completed, debounced, field, forCase, forField, looped, onCase, projection, silence, tapped, updates, with)
import PUI.HTML (body, provided, text)
import PUI.MDC (body1, button, card, elevation20, filledTextArea, filledTextField, headline6, indeterminateLinearProgress, segmentedButton, snackbar, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

main :: Effect Unit
main =
  body $ ( elevation20 Semigroupoid.do
      indeterminateLinearProgress # action loadOrder
      RecordToRecord.do
        headline6 text # projection ("Order " <> _) # forField @"shortId"
        card { caption: "Identifier" } $ RecordToRecord.do
          filledTextField { floatingLabel: "Short ID" } # asField @"shortId"
          filledTextField { floatingLabel: "Unique ID" } # asField @"orderId"
        card { caption: "Customer" }
          ( RecordToRecord.do
              filledTextField { floatingLabel: "First name" } # asField @"firstName"
              filledTextField { floatingLabel: "Last name" } # asField @"lastName") # field @"customer"
        card { caption: "Fulfillment" }
          ( ( Semigroupoid.do
                tabBar
                  [ { value: "dineIn", label: "Dine in" }
                  , { value: "takeaway", label: "Takeaway" }
                  , { value: "delivery", label: "Delivery" }
                  ] # asField @"selected" # completed
                filledTextField { floatingLabel: "Table" } # asField @"table" # provided # lcmap dineInPane # updates setTable
                filledTextField { floatingLabel: "Time" } # asField @"time" # provided # lcmap takeawayPane # updates setTime
                ( RecordToRecord.do
                    filledTextField { floatingLabel: "Address" } # asField @"address"
                    body1 text # projection distanceLine # forField @"address") # provided # lcmap deliveryPane # updates setAddress) # looped # dimap fulfillmentState fulfillmentCase) # field @"fulfillment"
        card { caption: "Total" } $ filledTextField { floatingLabel: "Total" } # asField @"total"
        card { caption: "Payment" }
          ( RecordToRecord.do
              segmentedButton
                [ { value: "cash", label: "Cash" }
                , { value: "card", label: "Card" }
                ] # asField @"selected" # dimap methodState methodCase # field @"method"
              filledTextField { floatingLabel: "Paid" } # asField @"paid"
              body1 text # projection paymentLine # forField @"method") # field @"payment"
        card { caption: "Remarks" } $ filledTextArea { columns: 80, rows: 3 } # asField @"remarks"
      body1 text # projection summarize # debounced # tapped
      RecordToVariant.do
        button { label: "Submit order", icon: "save" } # asCase @"submit"
        button { label: "Receipt", icon: "file" } # asCase @"printReceipt"
      VariantToVariant.do
        indeterminateLinearProgress # action submitOrder # onCase @"submit"
        indeterminateLinearProgress # action printReceipt # onCase @"printReceipt"
      VariantToRecord.do
        snackbar # forCase @"orderSubmitted"
        snackbar # forCase @"submissionFailed"
        snackbar # forCase @"receiptPrinted"
      silence
  ) # with unit

distanceKm :: String -> String
distanceKm address = show (length address)

distanceLine :: String -> String
distanceLine address = "Distance " <> distanceKm address <> " km"

paymentLine ::
  [ cash :: Unit
  , card :: Unit
  ]
  -> String
paymentLine method = "Paying by " <> methodText method

methodText ::
  [ cash :: Unit
  , card :: Unit
  ]
  -> String
methodText = match
  { cash: const "cash"
  , card: const "card"
  }

fulfillmentState ::
  [ dineIn :: { table :: String }
  , takeaway :: { time :: String }
  , delivery :: { address :: String }
  ]
  -> { selected :: String, table :: String, time :: String, address :: String }
fulfillmentState = match
  { dineIn: \r -> { selected: "dineIn", table: r.table, time: "12:00", address: "" }
  , takeaway: \r -> { selected: "takeaway", table: "1", time: r.time, address: "" }
  , delivery: \r -> { selected: "delivery", table: "1", time: "12:00", address: r.address }
  }

fulfillmentCase :: { selected :: String, table :: String, time :: String, address :: String } ->
  [ dineIn :: { table :: String }
  , takeaway :: { time :: String }
  , delivery :: { address :: String }
  ]
fulfillmentCase s =
  if s.selected == "dineIn" then .dineIn { table: s.table }
  else if s.selected == "takeaway" then .takeaway { time: s.time }
  else .delivery { address: s.address }

dineInPane :: { selected :: String, table :: String, time :: String, address :: String } -> Maybe { table :: String }
dineInPane s = if s.selected == "dineIn" then Just { table: s.table } else Nothing

takeawayPane :: { selected :: String, table :: String, time :: String, address :: String } -> Maybe { time :: String }
takeawayPane s = if s.selected == "takeaway" then Just { time: s.time } else Nothing

deliveryPane :: { selected :: String, table :: String, time :: String, address :: String } -> Maybe { address :: String }
deliveryPane s = if s.selected == "delivery" then Just { address: s.address } else Nothing

setTable :: { table :: String } -> { selected :: String, table :: String, time :: String, address :: String } -> { selected :: String, table :: String, time :: String, address :: String }
setTable { table } s = s { table = table }

setTime :: { time :: String } -> { selected :: String, table :: String, time :: String, address :: String } -> { selected :: String, table :: String, time :: String, address :: String }
setTime { time } s = s { time = time }

setAddress :: { address :: String } -> { selected :: String, table :: String, time :: String, address :: String } -> { selected :: String, table :: String, time :: String, address :: String }
setAddress { address } s = s { address = address }

methodState ::
  [ cash :: Unit
  , card :: Unit
  ]
  -> { selected :: Maybe String }
methodState = match
  { cash: const { selected: Just "cash" }
  , card: const { selected: Just "card" }
  }

methodCase :: { selected :: String } ->
  [ cash :: Unit
  , card :: Unit
  ]
methodCase r = if r.selected == "cash" then .cash unit else .card unit

summarize ::
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
  -> String
summarize order =
  "Summary: Order " <> order.shortId
    <> " (uniquely " <> order.orderId <> ")"
    <> " for " <> order.customer.firstName <> " " <> order.customer.lastName
    <> ", fulfilled as " <> fulfillmentText order.fulfillment
    <> ", paid " <> order.payment.paid <> " by " <> methodText order.payment.method
  where
  fulfillmentText = match
    { dineIn: \r -> "dine in at table " <> r.table
    , takeaway: \r -> "takeaway at " <> r.time
    , delivery: \r -> "delivery to " <> r.address <> " (" <> distanceKm r.address <> " km away)"
    }

loadOrder :: Unit -> Aff
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

submitOrder ::
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
  -> Aff
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

printReceipt ::
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
  -> Aff [ receiptPrinted :: String ]
printReceipt order = do
  liftEffect $ log $ "printing receipt for order " <> order.orderId
  delay (Milliseconds 2000.0)
  liftEffect $ log $ "printed receipt for order " <> order.orderId
  pure $ .receiptPrinted ("Receipt for order " <> order.shortId <> " printed")

