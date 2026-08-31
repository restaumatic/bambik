module OrderFormLogic (deliveryDistance, estimateDistance, fulfillmentCase, distanceOf, fulfillmentOf, fulfillmentState, loadOrder, presentOrder, printReceipt, receiptLine, rejectionLine, selection, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summarySettleTime) where

import Prelude ((<>), ($), (==), (/=), bind, const, discard, pure, show)

import Data.Variant (match)
import Data.Variant.Case (caseText)
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

setDistance :: { km :: Int, to :: String } -> { distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] } -> { distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
setDistance estimate _ = { distance: .estimated estimate }

staleDistanceForgotten :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] } -> { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
staleDistanceForgotten r@{ "Address": address, distance } = match
  { estimated: \e -> if e.to /= address then r { distance = .unknown {} } else r
  , unknown: \_ -> r
  } distance

distanceOf :: { distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] } -> [ estimated :: { kmText :: String }, unknown :: {} ]
distanceOf { distance } = match { estimated: \e -> .estimated { kmText: show e.km }, unknown: const (.unknown {}) } distance

fulfillmentOf ::
  { fulfillment ::
      [ "Dine in" :: { "Table" :: String }
      , "Takeaway" :: { "Time" :: String }
      , "Delivery" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
      ]
  }
  -> [ "Dine in" :: { "Table" :: String }
     , "Takeaway" :: { "Time" :: String }
     , "Delivery" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
     ]
fulfillmentOf { fulfillment } = fulfillment

deliveryDistance ::
  { fulfillment ::
      [ "Dine in" :: { "Table" :: String }
      , "Takeaway" :: { "Time" :: String }
      , "Delivery" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
      ]
  }
  -> [ estimated :: { kmText :: String }, unknown :: {} ]
deliveryDistance { fulfillment } = match { "Dine in": const (.unknown {}), "Takeaway": const (.unknown {}), "Delivery": \d -> distanceOf { distance: d.distance } } fulfillment

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

fulfillmentCase :: { selected :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ], "Table" :: String, "Time" :: String, "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] } ->
  [ "Dine in" :: { "Table" :: String }
  , "Takeaway" :: { "Time" :: String }
  , "Delivery" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
  ]
fulfillmentCase { selected, "Table": table, "Time": time, "Address": address, distance } = match
  { "Dine in": \_ -> ."Dine in" { "Table": table }
  , "Takeaway": \_ -> ."Takeaway" { "Time": time }
  , "Delivery": \_ -> ."Delivery" { "Address": address, distance }
  } selected

selection :: { selected :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ] } -> [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ]
selection = _.selected

presentOrder ::
  { payment ::
      { "Method" ::
          [ "cash" :: {}
          , "card" :: {}
          ]
      , "Paid" :: String
      , methodText :: String
      }
  , paidLine :: String
  }
  -> { payment ::
         { "Method" ::
             [ "cash" :: {}
             , "card" :: {}
             ]
         , "Paid" :: String
         , methodText :: String
         }
     , paidLine :: String
     }
presentOrder r = r
  { payment = r.payment { methodText = caseText r.payment."Method" }
  , paidLine = ", paid " <> r.payment."Paid" <> " by " <> caseText r.payment."Method"
  }

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
      , "Delivery" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
      ]
  , "Total" :: String
  , payment ::
      { "Method" ::
          [ "cash" :: {}
          , "card" :: {}
          ]
      , "Paid" :: String
      , methodText :: String
      }
  , "Remarks" :: String
  , paidLine :: String
  }
loadOrder _ = do
  liftEffect $ log "loading order"
  delay (Milliseconds 1000.0)
  liftEffect $ log "loaded order"
  let presented = presentOrder { payment: { "Method": ."cash" {}, "Paid": "0.00", methodText: "" }, paidLine: "" }
  pure
    { "Short ID": "7"
    , "Unique ID": "4617821"
    , customer:
        { "First name": "John"
        , "Last name": "Doe"
        }
    , fulfillment: ."Takeaway" { "Time": "8:30" }
    , "Total": "12.30"
    , payment: presented.payment
    , "Remarks": "Very spicy, please!"
    , paidLine: presented.paidLine
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
