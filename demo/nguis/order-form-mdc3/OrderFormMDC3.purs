module OrderFormMDC3 (orderFormMDC3) where

import Prelude ((#), ($), (<>), (==), Unit, const, discard, pure, show, unit)

import Data.Maybe (Maybe(..))
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
import PUI (PUI, action, asCase, asField, bracketed, completed, debounced, displayed, field, forCase, forField, forValue, looped, onCase, projection, required, silence, tapped, updates, with)
import Data.Profunctor.Row (widenRecordInput)
import PUI.HTML (body, provided, staticText, text)
import PUI.Web (Web)
import PUI.MDC3 (bodyLarge, button, card, elevation5, filledTextArea, filledTextField, headlineSmall, indeterminateLinearProgress, segmentedButton, snackbar, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderFormMDC3 :: Effect Unit
orderFormMDC3 =
  body $ ( elevation5 Semigroupoid.do
      indeterminateLinearProgress # action loadOrder
      RecordToRecord.do
        headlineSmall ( RecordToRecord.do
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
                  [ { value: .dineIn {}, label: "Dine in" }
                  , { value: .takeaway {}, label: "Takeaway" }
                  , { value: .delivery {}, label: "Delivery" }
                  ] # asField @"selected" # completed
                filledTextField { floatingLabel: "Table" } # asField @"table" # provided dineInPane # updates setTable
                filledTextField { floatingLabel: "Time" } # asField @"time" # provided takeawayPane # updates setTime
                ( RecordToRecord.do
                    filledTextField { floatingLabel: "Address" } # asField @"address"
                    bodyLarge ( RecordToRecord.do
                        staticText "Distance "
                        text # projection distanceKm # forField @"address"
                        staticText " km" )) # provided deliveryPane # updates setAddress) # bracketed fulfillmentState fulfillmentCase) # field @"fulfillment"
        card { caption: "Total" } $ filledTextField { floatingLabel: "Total" } # asField @"total"
        card { caption: "Payment" }
          ( RecordToRecord.do
              segmentedButton
                [ { value: .cash {}, label: "Cash" }
                , { value: .card {}, label: "Card" }
                ] # required # asField @"method"
              filledTextField { floatingLabel: "Paid" } # asField @"paid"
              bodyLarge ( RecordToRecord.do
                  staticText "Paying by "
                  text # projection methodText # forField @"method" )) # field @"payment"
        card { caption: "Remarks" } $ filledTextArea { columns: 80, rows: 3 } # asField @"remarks"
      bodyLarge ( Semigroupoid.do
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
              text # forValue # forField @"table" ) # provided dineInDetail # displayed
          ( RecordToRecord.do
              staticText "takeaway at "
              text # forValue # forField @"time" ) # provided takeawayDetail # displayed
          ( RecordToRecord.do
              staticText "delivery to "
              text # forValue # forField @"address"
              staticText " ("
              text # projection distanceKm # forField @"address"
              staticText " km away)" ) # provided deliveryDetail # displayed
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
        submittedToast
        rejectionToast
        receiptToast
      silence
  ) # with unit

distanceKm :: String -> String
distanceKm address = show (length address)

customerName :: { firstName :: String, lastName :: String } -> String
customerName { firstName, lastName } = firstName <> " " <> lastName

dineInDetail ::
  { fulfillment ::
      [ dineIn :: { table :: String }
      , takeaway :: { time :: String }
      , delivery :: { address :: String }
      ]
  }
  -> Maybe { table :: String }
dineInDetail { fulfillment } = match { dineIn: Just, takeaway: const Nothing, delivery: const Nothing } fulfillment

takeawayDetail ::
  { fulfillment ::
      [ dineIn :: { table :: String }
      , takeaway :: { time :: String }
      , delivery :: { address :: String }
      ]
  }
  -> Maybe { time :: String }
takeawayDetail { fulfillment } = match { dineIn: const Nothing, takeaway: Just, delivery: const Nothing } fulfillment

deliveryDetail ::
  { fulfillment ::
      [ dineIn :: { table :: String }
      , takeaway :: { time :: String }
      , delivery :: { address :: String }
      ]
  }
  -> Maybe { address :: String }
deliveryDetail { fulfillment } = match { dineIn: const Nothing, takeaway: const Nothing, delivery: Just } fulfillment

methodText ::
  [ cash :: {}
  , card :: {}
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
  -> { selected :: [ dineIn :: {}, takeaway :: {}, delivery :: {} ], table :: String, time :: String, address :: String }
fulfillmentState = match
  { dineIn: \r -> { selected: .dineIn {}, table: r.table, time: "12:00", address: "" }
  , takeaway: \r -> { selected: .takeaway {}, table: "1", time: r.time, address: "" }
  , delivery: \r -> { selected: .delivery {}, table: "1", time: "12:00", address: r.address }
  }

fulfillmentCase :: { selected :: [ dineIn :: {}, takeaway :: {}, delivery :: {} ], table :: String, time :: String, address :: String } ->
  [ dineIn :: { table :: String }
  , takeaway :: { time :: String }
  , delivery :: { address :: String }
  ]
fulfillmentCase { selected, table, time, address } = match
  { dineIn: \_ -> .dineIn { table }
  , takeaway: \_ -> .takeaway { time }
  , delivery: \_ -> .delivery { address }
  } selected

dineInPane :: { selected :: [ dineIn :: {}, takeaway :: {}, delivery :: {} ], table :: String } -> Maybe { table :: String }
dineInPane { selected, table } = match { dineIn: \_ -> Just { table }, takeaway: const Nothing, delivery: const Nothing } selected

takeawayPane :: { selected :: [ dineIn :: {}, takeaway :: {}, delivery :: {} ], time :: String } -> Maybe { time :: String }
takeawayPane { selected, time } = match { dineIn: const Nothing, takeaway: \_ -> Just { time }, delivery: const Nothing } selected

deliveryPane :: { selected :: [ dineIn :: {}, takeaway :: {}, delivery :: {} ], address :: String } -> Maybe { address :: String }
deliveryPane { selected, address } = match { dineIn: const Nothing, takeaway: const Nothing, delivery: \_ -> Just { address } } selected

setTable :: { table :: String } -> { table :: String } -> { table :: String }
setTable { table } _ = { table }

setTime :: { time :: String } -> { time :: String } -> { time :: String }
setTime { time } _ = { time }

setAddress :: { address :: String } -> { address :: String } -> { address :: String }
setAddress { address } _ = { address }

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
          [ cash :: {}
          , card :: {}
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
        { method: .cash {}
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
    [ orderSubmitted :: { shortId :: String }
    , submissionFailed :: { shortId :: String, reason :: String }
    ]
submitOrder { shortId, orderId, total } = do
  liftEffect $ log $ "submitting order " <> orderId
  delay (Milliseconds 1000.0)
  if total == ""
    then do
      liftEffect $ log "order submission failed"
      pure $ .submissionFailed { shortId, reason: "missing total" }
    else do
      liftEffect $ log "submitted order"
      pure $ .orderSubmitted { shortId }

submittedToast :: PUI Web [ orderSubmitted :: { shortId :: String } ] {}
submittedToast = snackbar # forCase @"orderSubmitted" (\{ shortId } -> "Order " <> shortId <> " submitted")

rejectionToast :: PUI Web [ submissionFailed :: { shortId :: String, reason :: String } ] {}
rejectionToast = snackbar # forCase @"submissionFailed" (\{ shortId, reason } -> "Order " <> shortId <> " rejected: " <> reason)

receiptToast :: PUI Web [ receiptPrinted :: { shortId :: String } ] {}
receiptToast = snackbar # forCase @"receiptPrinted" (\{ shortId } -> "Receipt for order " <> shortId <> " printed")

printReceipt ::
  { shortId :: String
  , orderId :: String
  }
  -> Aff [ receiptPrinted :: { shortId :: String } ]
printReceipt { shortId, orderId } = do
  liftEffect $ log $ "printing receipt for order " <> orderId
  delay (Milliseconds 2000.0)
  liftEffect $ log $ "printed receipt for order " <> orderId
  pure $ .receiptPrinted { shortId }

