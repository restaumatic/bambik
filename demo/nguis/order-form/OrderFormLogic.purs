module OrderFormLogic (deliveryDetail, deliveryDistance, dineInDetail, estimateDistance, fulfillmentCase, fulfillmentState, knownDistance, loadOrder, printReceipt, receiptLine, rejectionLine, selection, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summarySettleTime, takeawayDetail) where

import Prelude ((<>), (<$>), ($), (==), (/=), bind, const, discard, pure, show)

import Data.Maybe (Maybe(..))
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Random (randomInt)

estimateDistance :: { "Address" :: String } -> Aff [ estimated :: { km :: Int, to :: String } ]
estimateDistance { "Address": address } = do
  liftEffect $ log $ "estimating the distance to " <> address
  delay (Milliseconds 700.0)
  km <- liftEffect $ randomInt 1 6
  liftEffect $ log $ "estimated " <> show km <> " km"
  pure $ .estimated { km, to: address }

setDistance :: { km :: Int, to :: String } -> { distance :: Maybe { km :: Int, to :: String } } -> { distance :: Maybe { km :: Int, to :: String } }
setDistance estimate _ = { distance: Just estimate }

staleDistanceForgotten :: { "Address" :: String, distance :: Maybe { km :: Int, to :: String } } -> { "Address" :: String, distance :: Maybe { km :: Int, to :: String } }
staleDistanceForgotten r@{ "Address": address, distance: Just { to } } | to /= address = r { distance = Nothing }
staleDistanceForgotten r = r

knownDistance :: { distance :: Maybe { km :: Int, to :: String } } -> Maybe { km :: Int }
knownDistance { distance } = (\e -> { km: e.km }) <$> distance

dineInDetail ::
  { fulfillment ::
      [ "Dine in" :: { "Table" :: String }
      , "Takeaway" :: { "Time" :: String }
      , "Delivery" :: { "Address" :: String, distance :: Maybe { km :: Int, to :: String } }
      ]
  }
  -> Maybe { "Table" :: String }
dineInDetail { fulfillment } = match { "Dine in": Just, "Takeaway": const Nothing, "Delivery": const Nothing } fulfillment

takeawayDetail ::
  { fulfillment ::
      [ "Dine in" :: { "Table" :: String }
      , "Takeaway" :: { "Time" :: String }
      , "Delivery" :: { "Address" :: String, distance :: Maybe { km :: Int, to :: String } }
      ]
  }
  -> Maybe { "Time" :: String }
takeawayDetail { fulfillment } = match { "Dine in": const Nothing, "Takeaway": Just, "Delivery": const Nothing } fulfillment

deliveryDetail ::
  { fulfillment ::
      [ "Dine in" :: { "Table" :: String }
      , "Takeaway" :: { "Time" :: String }
      , "Delivery" :: { "Address" :: String, distance :: Maybe { km :: Int, to :: String } }
      ]
  }
  -> Maybe { "Address" :: String }
deliveryDetail { fulfillment } = match { "Dine in": const Nothing, "Takeaway": const Nothing, "Delivery": \r -> Just { "Address": r."Address" } } fulfillment

deliveryDistance ::
  { fulfillment ::
      [ "Dine in" :: { "Table" :: String }
      , "Takeaway" :: { "Time" :: String }
      , "Delivery" :: { "Address" :: String, distance :: Maybe { km :: Int, to :: String } }
      ]
  }
  -> Maybe { km :: Int }
deliveryDistance { fulfillment } = match { "Dine in": const Nothing, "Takeaway": const Nothing, "Delivery": \r -> (\e -> { km: e.km }) <$> r.distance } fulfillment

fulfillmentState ::
  [ "Dine in" :: { "Table" :: String }
  , "Takeaway" :: { "Time" :: String }
  , "Delivery" :: { "Address" :: String, distance :: Maybe { km :: Int, to :: String } }
  ]
  -> { selected :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ], "Table" :: String, "Time" :: String, "Address" :: String, distance :: Maybe { km :: Int, to :: String } }
fulfillmentState = match
  { "Dine in": \r -> { selected: ."Dine in" {}, "Table": r."Table", "Time": "12:00", "Address": "", distance: Nothing }
  , "Takeaway": \r -> { selected: ."Takeaway" {}, "Table": "1", "Time": r."Time", "Address": "", distance: Nothing }
  , "Delivery": \r -> { selected: ."Delivery" {}, "Table": "1", "Time": "12:00", "Address": r."Address", distance: r.distance }
  }

fulfillmentCase :: { selected :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ], "Table" :: String, "Time" :: String, "Address" :: String, distance :: Maybe { km :: Int, to :: String } } ->
  [ "Dine in" :: { "Table" :: String }
  , "Takeaway" :: { "Time" :: String }
  , "Delivery" :: { "Address" :: String, distance :: Maybe { km :: Int, to :: String } }
  ]
fulfillmentCase { selected, "Table": table, "Time": time, "Address": address, distance } = match
  { "Dine in": \_ -> ."Dine in" { "Table": table }
  , "Takeaway": \_ -> ."Takeaway" { "Time": time }
  , "Delivery": \_ -> ."Delivery" { "Address": address, distance }
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
      , "Delivery" :: { "Address" :: String, distance :: Maybe { km :: Int, to :: String } }
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
