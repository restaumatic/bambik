module OrderFormLogic (distanceLine, distanceOf, estimateDistance, fulfillmentCase, fulfillmentState, loadOrder, orderLine, payingLine, printReceipt, receiptLine, rejectionLine, selection, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summaryLine, summarySettleTime) where

import Prelude ((<>), ($), (==), (/=), bind, const, discard, pure, show)

import Data.Variant (match)
import Data.Variant.Case (caseText)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Random (randomInt)

estimateDistance :: forall r1. { "Address" :: String | r1 } -> Aff [ estimated :: { km :: Int, to :: String } ]
estimateDistance { "Address": address } = do
  liftEffect $ log $ "estimating the distance to " <> address
  delay (Milliseconds 700.0)
  km <- liftEffect $ randomInt 1 6
  liftEffect $ log $ "estimated " <> show km <> " km"
  pure $ .estimated { km, to: address }

setDistance :: forall r1 r2. { km :: Int, to :: String | r1 } -> { distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] | r2 } -> { distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] | r2 }
setDistance estimate order = order { distance = .estimated { km: estimate.km, to: estimate.to } }

staleDistanceForgotten :: forall r1. { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] | r1 } -> { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] | r1 }
staleDistanceForgotten r@{ "Address": address, distance } = match
  { estimated: \e -> if e.to /= address then r { distance = .unknown {} } else r
  , unknown: \_ -> r
  } distance

distanceOf :: forall r1. { distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] | r1 } -> [ estimated :: { km :: Int }, unknown :: {} ]
distanceOf { distance } = match { estimated: \e -> .estimated { km: e.km }, unknown: const (.unknown {}) } distance

distanceLine :: forall r1. { km :: Int | r1 } -> String
distanceLine { km } = "Distance " <> show km <> " km"

fulfillmentState ::
  [ "Dine in" :: { "Table" :: String }
  , "Takeaway" :: { "Time" :: String }
  , "Delivery" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
  ]
  -> { selected :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ], "Table" :: String, "Time" :: String, "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
fulfillmentState = match
  { "Dine in": \r -> { selected: ."Dine in" {}, "Table": r."Table", "Time": "12:00", "Address": "", distance: .unknown {} }
  , "Takeaway": \r -> { selected: ."Takeaway" {}, "Table": "1", "Time": r."Time", "Address": "", distance: .unknown {} }
  , "Delivery": \r -> { selected: ."Delivery" {}, "Table": "1", "Time": "12:00", "Address": r."Address", distance: r.distance }
  }

fulfillmentCase :: forall r1. { selected :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ], "Table" :: String, "Time" :: String, "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] | r1 } -> [ "Dine in" :: { "Table" :: String } , "Takeaway" :: { "Time" :: String } , "Delivery" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] } ]
fulfillmentCase { selected, "Table": table, "Time": time, "Address": address, distance } = match
  { "Dine in": \_ -> ."Dine in" { "Table": table }
  , "Takeaway": \_ -> ."Takeaway" { "Time": time }
  , "Delivery": \_ -> ."Delivery" { "Address": address, distance }
  } selected

selection :: forall r1. { selected :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ] | r1 } -> [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ]
selection = _.selected

orderLine :: forall r1. { "Identifier" :: { "Short ID" :: String, "Unique ID" :: String } | r1 } -> String
orderLine r = "Order " <> r."Identifier"."Short ID"

summaryLine :: forall r1. { "Identifier" :: { "Short ID" :: String, "Unique ID" :: String } , "Customer" :: { "First name" :: String, "Last name" :: String } , "Fulfillment" :: { "Mode" :: [ "Dine in" :: { "Table" :: String } , "Takeaway" :: { "Time" :: String } , "Delivery" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] } ] } , "Payment" :: { "Total" :: String, "Method" :: [ "cash" :: {}, "card" :: {} ], "Paid" :: String } | r1 } -> String
summaryLine r = "Summary: Order " <> r."Identifier"."Short ID" <> " (uniquely " <> r."Identifier"."Unique ID" <> ") for " <> r."Customer"."First name" <> " " <> r."Customer"."Last name" <> ", fulfilled as " <> fulfillmentText r."Fulfillment"."Mode" <> ", paid " <> r."Payment"."Paid" <> " by " <> caseText r."Payment"."Method"

fulfillmentText ::
  [ "Dine in" :: { "Table" :: String }
  , "Takeaway" :: { "Time" :: String }
  , "Delivery" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
  ]
  -> String
fulfillmentText = match
  { "Dine in": \d -> "dine in at table " <> d."Table"
  , "Takeaway": \d -> "takeaway at " <> d."Time"
  , "Delivery": \d -> "delivery to " <> d."Address" <> awayText d.distance
  }

awayText :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] -> String
awayText = match
  { estimated: \e -> " (" <> show e.km <> " km away)"
  , unknown: const ""
  }

payingLine :: forall r1. { "Method" :: [ "cash" :: {}, "card" :: {} ] | r1 } -> String
payingLine r = "Paying by " <> caseText r."Method"

loadOrder :: forall r1. { | r1 } -> Aff { "Identifier" :: { "Short ID" :: String , "Unique ID" :: String } , "Customer" :: { "First name" :: String , "Last name" :: String } , "Fulfillment" :: { "Mode" :: [ "Dine in" :: { "Table" :: String } , "Takeaway" :: { "Time" :: String } , "Delivery" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] } ] } , "Payment" :: { "Total" :: String , "Method" :: [ "cash" :: {} , "card" :: {} ] , "Paid" :: String } , "Kitchen" :: { "Remarks" :: String } }
loadOrder _ = do
  liftEffect $ log "loading order"
  delay (Milliseconds 1000.0)
  liftEffect $ log "loaded order"
  pure
    { "Identifier":
      { "Short ID": "7"
      , "Unique ID": "4617821"
      }
    , "Customer":
      { "First name": "John"
      , "Last name": "Doe"
      }
    , "Fulfillment": { "Mode": ."Takeaway" { "Time": "8:30" } }
    , "Payment": { "Total": "12.30", "Method": ."cash" {}, "Paid": "0.00" }
    , "Kitchen": { "Remarks": "Very spicy, please!" }
    }

submitOrder :: forall r1. { "Identifier" :: { "Short ID" :: String, "Unique ID" :: String } , "Payment" :: { "Total" :: String, "Method" :: [ "cash" :: {}, "card" :: {} ], "Paid" :: String } | r1 } -> Aff [ orderSubmitted :: { "Short ID" :: String } , submissionFailed :: { "Short ID" :: String, reason :: String } ]
submitOrder { "Identifier": { "Short ID": shortId, "Unique ID": orderId }, "Payment": { "Total": total } } = do
  liftEffect $ log $ "submitting order " <> orderId
  delay (Milliseconds 1000.0)
  if total == ""
    then do
      liftEffect $ log "order submission failed"
      pure $ .submissionFailed { "Short ID": shortId, reason: "missing total" }
    else do
      liftEffect $ log "submitted order"
      pure $ .orderSubmitted { "Short ID": shortId }

submittedLine :: forall r1. { "Short ID" :: String | r1 } -> String
submittedLine { "Short ID": shortId } = "Order " <> shortId <> " submitted"

rejectionLine :: forall r1. { "Short ID" :: String, reason :: String | r1 } -> String
rejectionLine { "Short ID": shortId, reason } = "Order " <> shortId <> " rejected: " <> reason

printReceipt :: forall r1. { "Identifier" :: { "Short ID" :: String, "Unique ID" :: String } | r1 } -> Aff [ receiptPrinted :: { "Short ID" :: String } ]
printReceipt { "Identifier": { "Short ID": shortId, "Unique ID": orderId } } = do
  liftEffect $ log $ "printing receipt for order " <> orderId
  delay (Milliseconds 2000.0)
  liftEffect $ log $ "printed receipt for order " <> orderId
  pure $ .receiptPrinted { "Short ID": shortId }

receiptLine :: forall r1. { "Short ID" :: String | r1 } -> String
receiptLine { "Short ID": shortId } = "Receipt for order " <> shortId <> " printed"

summarySettleTime :: { ms :: Number }
summarySettleTime = { ms: 300.0 }
