module OrderFormLogic (deliveryDetail, deliveryPane, dineInDetail, dineInPane, distanceKm, fulfillmentCase, fulfillmentState, loadOrder, methodText, printReceipt, receiptLine, rejectionLine, setAddress, setTable, setTime, submitOrder, submittedLine, summarySettleTime, takeawayDetail, takeawayPane) where

import Prelude (($), (<>), (==), const, discard, pure, show)

import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)

distanceKm :: String -> String
distanceKm address = show (length address)

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

setTable :: { table :: String } -> { table :: String }
setTable { table } = { table }

setTime :: { time :: String } -> { time :: String }
setTime { time } = { time }

setAddress :: { address :: String } -> { address :: String }
setAddress { address } = { address }

loadOrder :: {} -> Aff
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

submittedLine :: { shortId :: String } -> String
submittedLine { shortId } = "Order " <> shortId <> " submitted"

rejectionLine :: { shortId :: String, reason :: String } -> String
rejectionLine { shortId, reason } = "Order " <> shortId <> " rejected: " <> reason

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

receiptLine :: { shortId :: String } -> String
receiptLine { shortId } = "Receipt for order " <> shortId <> " printed"

summarySettleTime :: { ms :: Number }
summarySettleTime = { ms: 300.0 }
