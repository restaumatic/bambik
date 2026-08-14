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
      [ dineIn :: { "Table" :: String }
      , takeaway :: { "Time" :: String }
      , delivery :: { "Address" :: String }
      ]
  }
  -> Maybe { "Table" :: String }
dineInDetail { fulfillment } = match { dineIn: Just, takeaway: const Nothing, delivery: const Nothing } fulfillment

takeawayDetail ::
  { fulfillment ::
      [ dineIn :: { "Table" :: String }
      , takeaway :: { "Time" :: String }
      , delivery :: { "Address" :: String }
      ]
  }
  -> Maybe { "Time" :: String }
takeawayDetail { fulfillment } = match { dineIn: const Nothing, takeaway: Just, delivery: const Nothing } fulfillment

deliveryDetail ::
  { fulfillment ::
      [ dineIn :: { "Table" :: String }
      , takeaway :: { "Time" :: String }
      , delivery :: { "Address" :: String }
      ]
  }
  -> Maybe { "Address" :: String }
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
  [ dineIn :: { "Table" :: String }
  , takeaway :: { "Time" :: String }
  , delivery :: { "Address" :: String }
  ]
  -> { selected :: [ dineIn :: {}, takeaway :: {}, delivery :: {} ], "Table" :: String, "Time" :: String, "Address" :: String }
fulfillmentState = match
  { dineIn: \r -> { selected: .dineIn {}, "Table": r."Table", "Time": "12:00", "Address": "" }
  , takeaway: \r -> { selected: .takeaway {}, "Table": "1", "Time": r."Time", "Address": "" }
  , delivery: \r -> { selected: .delivery {}, "Table": "1", "Time": "12:00", "Address": r."Address" }
  }

fulfillmentCase :: { selected :: [ dineIn :: {}, takeaway :: {}, delivery :: {} ], "Table" :: String, "Time" :: String, "Address" :: String } ->
  [ dineIn :: { "Table" :: String }
  , takeaway :: { "Time" :: String }
  , delivery :: { "Address" :: String }
  ]
fulfillmentCase { selected, "Table": table, "Time": time, "Address": address } = match
  { dineIn: \_ -> .dineIn { "Table": table }
  , takeaway: \_ -> .takeaway { "Time": time }
  , delivery: \_ -> .delivery { "Address": address }
  } selected

dineInPane :: { selected :: [ dineIn :: {}, takeaway :: {}, delivery :: {} ], "Table" :: String } -> Maybe { "Table" :: String }
dineInPane { selected, "Table": table } = match { dineIn: \_ -> Just { "Table": table }, takeaway: const Nothing, delivery: const Nothing } selected

takeawayPane :: { selected :: [ dineIn :: {}, takeaway :: {}, delivery :: {} ], "Time" :: String } -> Maybe { "Time" :: String }
takeawayPane { selected, "Time": time } = match { dineIn: const Nothing, takeaway: \_ -> Just { "Time": time }, delivery: const Nothing } selected

deliveryPane :: { selected :: [ dineIn :: {}, takeaway :: {}, delivery :: {} ], "Address" :: String } -> Maybe { "Address" :: String }
deliveryPane { selected, "Address": address } = match { dineIn: const Nothing, takeaway: const Nothing, delivery: \_ -> Just { "Address": address } } selected

setTable :: { "Table" :: String } -> { "Table" :: String }
setTable { "Table": table } = { "Table": table }

setTime :: { "Time" :: String } -> { "Time" :: String }
setTime { "Time": time } = { "Time": time }

setAddress :: { "Address" :: String } -> { "Address" :: String }
setAddress { "Address": address } = { "Address": address }

loadOrder :: {} -> Aff
  { shortId :: String
  , orderId :: String
  , customer ::
      { "First name" :: String
      , "Last name" :: String
      }
  , fulfillment ::
      [ dineIn :: { "Table" :: String }
      , takeaway :: { "Time" :: String }
      , delivery :: { "Address" :: String }
      ]
  , "Total" :: String
  , payment ::
      { "Method" ::
          [ cash :: {}
          , card :: {}
          ]
      , "Paid" :: String
      }
  , "Remarks" :: String
  }
loadOrder _ = do
  liftEffect $ log "loading order"
  delay (Milliseconds 1000.0)
  liftEffect $ log "loaded order"
  pure
    { shortId: "7"
    , orderId: "4617821"
    , customer:
        { "First name": "John"
        , "Last name": "Doe"
        }
    , fulfillment: .takeaway { "Time": "8:30" }
    , "Total": "12.30"
    , payment:
        { "Method": .cash {}
        , "Paid": "0.00"
        }
    , "Remarks": "Very spicy, please!"
    }

submitOrder ::
  { shortId :: String
  , orderId :: String
  , "Total" :: String
  }
  -> Aff
    [ orderSubmitted :: { shortId :: String }
    , submissionFailed :: { shortId :: String, reason :: String }
    ]
submitOrder { shortId, orderId, "Total": total } = do
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
