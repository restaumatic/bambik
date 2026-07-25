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
import PUI (action, asCase, asField, completed, debounced, displayed, field, forCase, forField, forValue, looped, onCase, projection, silence, tapped, updates, with)
import Data.Profunctor.Row (widenRecordInput)
import PUI.HTML (body, provided, staticText, text)
import PUI.MDC (body1, button, card, elevation20, filledTextArea, filledTextField, headline6, indeterminateLinearProgress, segmentedButton, snackbar, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

main :: Effect Unit
main =
  body $ ( elevation20 Semigroupoid.do
      indeterminateLinearProgress # action loadOrder
      RecordToRecord.do
        headline6 ( RecordToRecord.do
            staticText "Order "
            text # forValue # forField @"shortId" )
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
                    body1 ( RecordToRecord.do
                        staticText "Distance "
                        text # projection distanceKm # forField @"address"
                        staticText " km" )) # provided # lcmap deliveryPane # updates setAddress) # looped # dimap fulfillmentState fulfillmentCase) # field @"fulfillment"
        card { caption: "Total" } $ filledTextField { floatingLabel: "Total" } # asField @"total"
        card { caption: "Payment" }
          ( RecordToRecord.do
              segmentedButton
                [ { value: "cash", label: "Cash" }
                , { value: "card", label: "Card" }
                ] # asField @"selected" # dimap methodState methodCase # field @"method"
              filledTextField { floatingLabel: "Paid" } # asField @"paid"
              body1 ( RecordToRecord.do
                  staticText "Paying by "
                  text # projection methodText # forField @"method" )) # field @"payment"
        card { caption: "Remarks" } $ filledTextArea { columns: 80, rows: 3 } # asField @"remarks"
      body1 ( Semigroupoid.do
          ( RecordToRecord.do
              staticText "Summary: Order "
              text # forValue # forField @"shortId"
              staticText " (uniquely "
              text # forValue # forField @"orderId"
              staticText ") for "
              text # projection customerName # forField @"customer"
              staticText ", fulfilled as " ) # debounced # tapped
          ( RecordToRecord.do
              staticText "dine in at table "
              text # forValue # forField @"table" ) # provided # lcmap dineInDetail # displayed
          ( RecordToRecord.do
              staticText "takeaway at "
              text # forValue # forField @"time" ) # provided # lcmap takeawayDetail # displayed
          ( RecordToRecord.do
              staticText "delivery to "
              text # forValue # forField @"address"
              staticText " ("
              text # projection distanceKm # forField @"address"
              staticText " km away)" ) # provided # lcmap deliveryDetail # displayed
          ( RecordToRecord.do
              staticText ", paid "
              text # forValue # forField @"paid"
              staticText " by "
              text # projection methodText # forField @"method" ) # field @"payment" # debounced # tapped )
      ( RecordToVariant.do
          button { label: "Submit order", icon: "save" } # asCase @"submit"
          button { label: "Receipt", icon: "file" } # asCase @"printReceipt") # widenRecordInput
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

customerName :: { firstName :: String, lastName :: String } -> String
customerName c = c.firstName <> " " <> c.lastName

dineInDetail ::
  { fulfillment ::
      [ dineIn :: { table :: String }
      , takeaway :: { time :: String }
      , delivery :: { address :: String }
      ]
  }
  -> Maybe { table :: String }
dineInDetail order = match { dineIn: Just, takeaway: const Nothing, delivery: const Nothing } order.fulfillment

takeawayDetail ::
  { fulfillment ::
      [ dineIn :: { table :: String }
      , takeaway :: { time :: String }
      , delivery :: { address :: String }
      ]
  }
  -> Maybe { time :: String }
takeawayDetail order = match { dineIn: const Nothing, takeaway: Just, delivery: const Nothing } order.fulfillment

deliveryDetail ::
  { fulfillment ::
      [ dineIn :: { table :: String }
      , takeaway :: { time :: String }
      , delivery :: { address :: String }
      ]
  }
  -> Maybe { address :: String }
deliveryDetail order = match { dineIn: const Nothing, takeaway: const Nothing, delivery: Just } order.fulfillment

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

dineInPane :: { selected :: String, table :: String } -> Maybe { table :: String }
dineInPane s = if s.selected == "dineIn" then Just { table: s.table } else Nothing

takeawayPane :: { selected :: String, time :: String } -> Maybe { time :: String }
takeawayPane s = if s.selected == "takeaway" then Just { time: s.time } else Nothing

deliveryPane :: { selected :: String, address :: String } -> Maybe { address :: String }
deliveryPane s = if s.selected == "delivery" then Just { address: s.address } else Nothing

setTable :: { table :: String } -> { table :: String } -> { table :: String }
setTable { table } _ = { table }

setTime :: { time :: String } -> { time :: String } -> { time :: String }
setTime { time } _ = { time }

setAddress :: { address :: String } -> { address :: String } -> { address :: String }
setAddress { address } _ = { address }

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
  , total :: String
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
  }
  -> Aff [ receiptPrinted :: String ]
printReceipt order = do
  liftEffect $ log $ "printing receipt for order " <> order.orderId
  delay (Milliseconds 2000.0)
  liftEffect $ log $ "printed receipt for order " <> order.orderId
  pure $ .receiptPrinted ("Receipt for order " <> order.shortId <> " printed")

