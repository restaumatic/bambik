module OrderForm (Distance, Estimate, Fulfillment, FulfillmentMode, Order, Payment, distanceLine, estimateDistance, fulfillmentMode, loadOrder, orderLine, payingLine, printReceipt, setDistance, staleDistanceForgotten, submitOrder, summaryLine) where

import Prelude ((<>), ($), (==), (/=), bind, const, discard, pure, show)

import Data.String (toLower)
import Data.Variant (match)
import Data.Variant.Case (caseText)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Random (randomInt)

type Order =
  { "Customer" :: { "First name" :: String, "Last name" :: String }
  , "Fulfillment" :: Fulfillment
  , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
  , "Kitchen" :: { "Remarks" :: String }
  , "Payment" :: Payment
  }

type Fulfillment =
  { "Address" :: String
  , "Mode" :: FulfillmentMode
  , "Table" :: String
  , "Time" :: String
  , distance :: Distance
  }

type FulfillmentMode = [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]

type Distance = [ estimated :: Estimate, unknown :: {} ]

type Estimate = { km :: Int, to :: String }

type Payment = { "Method" :: [ "Card" :: {}, "Cash" :: {} ], "Paid" :: String, "Total" :: String }

estimateDistance :: Order -> Aff [ "Distance estimated" :: Estimate ]
estimateDistance { "Fulfillment": { "Address": address } } = do
  liftEffect $ log $ "estimating the distance to " <> address
  delay (Milliseconds 700.0)
  km <- liftEffect $ randomInt 1 6
  liftEffect $ log $ "estimated " <> show km <> " km"
  pure $ ."Distance estimated" { km, to: address }

setDistance :: { event :: Estimate, model :: Order } -> Order
setDistance { event, model: order } = order { "Fulfillment" = order."Fulfillment" { distance = .estimated event } }

fulfillmentMode :: Order -> FulfillmentMode
fulfillmentMode order = order."Fulfillment"."Mode"

staleDistanceForgotten :: Fulfillment -> Fulfillment
staleDistanceForgotten r@{ "Address": address, distance } = match
  { estimated: \e -> if e.to /= address then r { distance = .unknown {} } else r
  , unknown: \_ -> r
  } distance

distanceLine :: Fulfillment -> String
distanceLine { distance } = match { estimated: \e -> "Distance " <> show e.km <> " km", unknown: const "" } distance

orderLine :: Order -> String
orderLine r = "Order " <> r."Identifier"."Short ID"

summaryLine :: Order -> String
summaryLine r =
  "Summary: Order " <> r."Identifier"."Short ID" <> " (ID " <> r."Identifier"."Unique ID" <> ")"
    <> " for " <> r."Customer"."First name" <> " " <> r."Customer"."Last name"
    <> ", fulfilled as " <> fulfillmentText r."Fulfillment"
    <> ", total " <> r."Payment"."Total"
    <> ", paid " <> r."Payment"."Paid" <> " by " <> toLower (caseText r."Payment"."Method")

fulfillmentText :: Fulfillment -> String
fulfillmentText f = match
  { "Dine in": \_ -> "dine in at table " <> f."Table"
  , "Takeaway": \_ -> "takeaway at " <> f."Time"
  , "Delivery": \_ -> "delivery to " <> f."Address" <> awayText f.distance
  } f."Mode"

awayText :: Distance -> String
awayText = match
  { estimated: \e -> " (" <> show e.km <> " km away)"
  , unknown: const ""
  }

payingLine :: Payment -> String
payingLine r = "Paying by " <> toLower (caseText r."Method")

loadOrder :: {} -> Aff [ "Order loaded" :: Order ]
loadOrder _ = do
  liftEffect $ log "loading order"
  delay (Milliseconds 1000.0)
  liftEffect $ log "loaded order"
  pure $ ."Order loaded"
    { "Identifier":
      { "Short ID": "7"
      , "Unique ID": "4617821"
      }
    , "Customer":
      { "First name": "John"
      , "Last name": "Doe"
      }
    , "Fulfillment": { "Mode": ."Takeaway" {}, "Table": "1", "Time": "8:30", "Address": "", distance: .unknown {} }
    , "Payment": { "Total": "12.30", "Method": ."Cash" {}, "Paid": "0.00" }
    , "Kitchen": { "Remarks": "Very spicy, please!" }
    }

submitOrder :: Order -> Aff [ "Total missing" :: {}, "Order submitted" :: {} ]
submitOrder { "Identifier": { "Unique ID": orderId }, "Payment": { "Total": total } } = do
  liftEffect $ log $ "submitting order " <> orderId
  delay (Milliseconds 1000.0)
  if total == ""
    then do
      liftEffect $ log "order submission failed"
      pure $ ."Total missing" {}
    else do
      liftEffect $ log "submitted order"
      pure $ ."Order submitted" {}

printReceipt :: Order -> Aff [ "Receipt printed" :: {} ]
printReceipt { "Identifier": { "Unique ID": orderId } } = do
  liftEffect $ log $ "printing receipt for order " <> orderId
  delay (Milliseconds 2000.0)
  liftEffect $ log $ "printed receipt for order " <> orderId
  pure $ ."Receipt printed" {}
