module OrderFormLogic (deliveryDetail, deliveryLine, dineInDetail, dineInLine, distanceKm, distanceLine, fulfillmentCase, fulfillmentState, headerLine, loadOrder, payingLine, paymentLine, printReceipt, receiptLine, rejectionLine, selection, submitOrder, submittedLine, summaryLead, summarySettleTime, takeawayDetail, takeawayLine) where

import Data.Variant.Case (caseText)
import Prelude ((<>), ($), (==), const, discard, pure, show)

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
      [ "Dine in" :: { "Table" :: String }
      , "Takeaway" :: { "Time" :: String }
      , "Delivery" :: { "Address" :: String }
      ]
  }
  -> Maybe { "Table" :: String }
dineInDetail { fulfillment } = match { "Dine in": Just, "Takeaway": const Nothing, "Delivery": const Nothing } fulfillment

takeawayDetail ::
  { fulfillment ::
      [ "Dine in" :: { "Table" :: String }
      , "Takeaway" :: { "Time" :: String }
      , "Delivery" :: { "Address" :: String }
      ]
  }
  -> Maybe { "Time" :: String }
takeawayDetail { fulfillment } = match { "Dine in": const Nothing, "Takeaway": Just, "Delivery": const Nothing } fulfillment

deliveryDetail ::
  { fulfillment ::
      [ "Dine in" :: { "Table" :: String }
      , "Takeaway" :: { "Time" :: String }
      , "Delivery" :: { "Address" :: String }
      ]
  }
  -> Maybe { "Address" :: String }
deliveryDetail { fulfillment } = match { "Dine in": const Nothing, "Takeaway": const Nothing, "Delivery": Just } fulfillment

fulfillmentState ::
  [ "Dine in" :: { "Table" :: String }
  , "Takeaway" :: { "Time" :: String }
  , "Delivery" :: { "Address" :: String }
  ]
  -> { selected :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ], "Table" :: String, "Time" :: String, "Address" :: String }
fulfillmentState = match
  { "Dine in": \r -> { selected: ."Dine in" {}, "Table": r."Table", "Time": "12:00", "Address": "" }
  , "Takeaway": \r -> { selected: ."Takeaway" {}, "Table": "1", "Time": r."Time", "Address": "" }
  , "Delivery": \r -> { selected: ."Delivery" {}, "Table": "1", "Time": "12:00", "Address": r."Address" }
  }

fulfillmentCase :: { selected :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ], "Table" :: String, "Time" :: String, "Address" :: String } ->
  [ "Dine in" :: { "Table" :: String }
  , "Takeaway" :: { "Time" :: String }
  , "Delivery" :: { "Address" :: String }
  ]
fulfillmentCase { selected, "Table": table, "Time": time, "Address": address } = match
  { "Dine in": \_ -> ."Dine in" { "Table": table }
  , "Takeaway": \_ -> ."Takeaway" { "Time": time }
  , "Delivery": \_ -> ."Delivery" { "Address": address }
  } selected

selection :: { selected :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ] } -> [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ]
selection = _.selected

loadOrder :: {} -> Aff
  { "Short ID" :: String
  , "Unique ID" :: String
  , customer ::
      { "First name" :: String
      , "Last name" :: String
      }
  , fulfillment ::
      [ "Dine in" :: { "Table" :: String }
      , "Takeaway" :: { "Time" :: String }
      , "Delivery" :: { "Address" :: String }
      ]
  , "Total" :: String
  , payment ::
      { "Method" ::
          [ "cash" :: {}
          , "card" :: {}
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
    { "Short ID": "7"
    , "Unique ID": "4617821"
    , customer:
        { "First name": "John"
        , "Last name": "Doe"
        }
    , fulfillment: ."Takeaway" { "Time": "8:30" }
    , "Total": "12.30"
    , payment:
        { "Method": ."cash" {}
        , "Paid": "0.00"
        }
    , "Remarks": "Very spicy, please!"
    }

submitOrder ::
  { "Short ID" :: String
  , "Unique ID" :: String
  , "Total" :: String
  }
  -> Aff
    [ orderSubmitted :: { "Short ID" :: String }
    , submissionFailed :: { "Short ID" :: String, reason :: String }
    ]
submitOrder { "Short ID": shortId, "Unique ID": orderId, "Total": total } = do
  liftEffect $ log $ "submitting order " <> orderId
  delay (Milliseconds 1000.0)
  if total == ""
    then do
      liftEffect $ log "order submission failed"
      pure $ .submissionFailed { "Short ID": shortId, reason: "missing total" }
    else do
      liftEffect $ log "submitted order"
      pure $ .orderSubmitted { "Short ID": shortId }

submittedLine :: { "Short ID" :: String } -> String
submittedLine { "Short ID": shortId } = "Order " <> shortId <> " submitted"

rejectionLine :: { "Short ID" :: String, reason :: String } -> String
rejectionLine { "Short ID": shortId, reason } = "Order " <> shortId <> " rejected: " <> reason

printReceipt ::
  { "Short ID" :: String
  , "Unique ID" :: String
  }
  -> Aff [ receiptPrinted :: { "Short ID" :: String } ]
printReceipt { "Short ID": shortId, "Unique ID": orderId } = do
  liftEffect $ log $ "printing receipt for order " <> orderId
  delay (Milliseconds 2000.0)
  liftEffect $ log $ "printed receipt for order " <> orderId
  pure $ .receiptPrinted { "Short ID": shortId }

receiptLine :: { "Short ID" :: String } -> String
receiptLine { "Short ID": shortId } = "Receipt for order " <> shortId <> " printed"

summarySettleTime :: { ms :: Number }
summarySettleTime = { ms: 300.0 }

headerLine :: { "Short ID" :: String } -> String
headerLine { "Short ID": shortId } = "Order " <> shortId

distanceLine :: { "Address" :: String } -> String
distanceLine { "Address": address } = "Distance " <> distanceKm address <> " km"

payingLine :: { "Method" :: [ cash :: {}, card :: {} ] } -> String
payingLine { "Method": method } = "Paying by " <> caseText method

summaryLead :: { "Short ID" :: String, "Unique ID" :: String, customer :: { "First name" :: String, "Last name" :: String } } -> String
summaryLead { "Short ID": shortId, "Unique ID": uniqueId, customer } =
  "Summary: Order " <> shortId <> " (uniquely " <> uniqueId <> ") for "
    <> customer."First name" <> " " <> customer."Last name" <> ", fulfilled as "

dineInLine :: { "Table" :: String } -> String
dineInLine { "Table": table } = "dine in at table " <> table

takeawayLine :: { "Time" :: String } -> String
takeawayLine { "Time": time } = "takeaway at " <> time

deliveryLine :: { "Address" :: String } -> String
deliveryLine { "Address": address } = "delivery to " <> address <> " (" <> distanceKm address <> " km away)"

paymentLine :: { payment :: { "Method" :: [ cash :: {}, card :: {} ], "Paid" :: String } } -> String
paymentLine { payment } = ", paid " <> payment."Paid" <> " by " <> caseText payment."Method"
