module OrderForm (distanceLine, estimateDistance, fulfillmentMode, loadOrder, orderLine, orderLoadedLine, payingLine, printReceipt, receiptLine, rejectionLine, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summaryLine) where

import Prelude ((<>), ($), (==), (/=), bind, const, discard, pure, show)

import Data.Variant (match)
import Data.Variant.Case (caseText)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Random (randomInt)

estimateDistance
  :: { "Customer" :: { "First name" :: String, "Last name" :: String }
     , "Fulfillment" :: { "Address" :: String
                        , "Mode" :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
                        , "Table" :: String
                        , "Time" :: String
                        , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
                        }
     , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
     , "Kitchen" :: { "Remarks" :: String }
     , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
     }
  -> Aff [ "Distance estimated" :: { km :: Int, to :: String } ]
estimateDistance { "Fulfillment": { "Address": address } } = do
  liftEffect $ log $ "estimating the distance to " <> address
  delay (Milliseconds 700.0)
  km <- liftEffect $ randomInt 1 6
  liftEffect $ log $ "estimated " <> show km <> " km"
  pure $ ."Distance estimated" { km, to: address }

setDistance
  :: { event :: { km :: Int, to :: String }
     , model :: { "Customer" :: { "First name" :: String, "Last name" :: String }
                , "Fulfillment" :: { "Address" :: String
                                   , "Mode" :: [ "Delivery" :: {}
                                               , "Dine in" :: {}
                                               , "Takeaway" :: {}
                                               ]
                                   , "Table" :: String
                                   , "Time" :: String
                                   , distance :: [ estimated :: { km :: Int, to :: String }
                                                 , unknown :: {}
                                                 ]
                                   }
                , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
                , "Kitchen" :: { "Remarks" :: String }
                , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ]
                               , "Paid" :: String
                               , "Total" :: String
                               }
                }
     }
  -> { "Customer" :: { "First name" :: String, "Last name" :: String }
     , "Fulfillment" :: { "Address" :: String
                        , "Mode" :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
                        , "Table" :: String
                        , "Time" :: String
                        , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
                        }
     , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
     , "Kitchen" :: { "Remarks" :: String }
     , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
     }
setDistance { event, model: order } = order { "Fulfillment" = order."Fulfillment" { distance = .estimated event } }

fulfillmentMode
  :: { "Customer" :: { "First name" :: String, "Last name" :: String }
     , "Fulfillment" :: { "Address" :: String
                        , "Mode" :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
                        , "Table" :: String
                        , "Time" :: String
                        , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
                        }
     , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
     , "Kitchen" :: { "Remarks" :: String }
     , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
     }
  -> [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
fulfillmentMode order = order."Fulfillment"."Mode"

staleDistanceForgotten
  :: { "Address" :: String
     , "Mode" :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
     , "Table" :: String
     , "Time" :: String
     , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
     }
  -> { "Address" :: String
     , "Mode" :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
     , "Table" :: String
     , "Time" :: String
     , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
     }
staleDistanceForgotten r@{ "Address": address, distance } = match
  { estimated: \e -> if e.to /= address then r { distance = .unknown {} } else r
  , unknown: \_ -> r
  } distance

distanceLine
  :: { "Address" :: String
     , "Mode" :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
     , "Table" :: String
     , "Time" :: String
     , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
     }
  -> String
distanceLine { distance } = match { estimated: \e -> "Distance " <> show e.km <> " km", unknown: const "" } distance

orderLine
  :: { "Customer" :: { "First name" :: String, "Last name" :: String }
     , "Fulfillment" :: { "Address" :: String
                        , "Mode" :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
                        , "Table" :: String
                        , "Time" :: String
                        , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
                        }
     , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
     , "Kitchen" :: { "Remarks" :: String }
     , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
     }
  -> String
orderLine r = "Order " <> r."Identifier"."Short ID"

orderLoadedLine
  :: { "Customer" :: { "First name" :: String, "Last name" :: String }
     , "Fulfillment" :: { "Address" :: String
                        , "Mode" :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
                        , "Table" :: String
                        , "Time" :: String
                        , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
                        }
     , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
     , "Kitchen" :: { "Remarks" :: String }
     , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
     }
  -> String
orderLoadedLine r = "Loaded order " <> r."Identifier"."Short ID" <> " for " <> r."Customer"."First name" <> " " <> r."Customer"."Last name"

summaryLine
  :: { "Customer" :: { "First name" :: String, "Last name" :: String }
     , "Fulfillment" :: { "Address" :: String
                        , "Mode" :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
                        , "Table" :: String
                        , "Time" :: String
                        , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
                        }
     , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
     , "Kitchen" :: { "Remarks" :: String }
     , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
     }
  -> String
summaryLine r = "Summary: Order " <> r."Identifier"."Short ID" <> " (uniquely " <> r."Identifier"."Unique ID" <> ") for " <> r."Customer"."First name" <> " " <> r."Customer"."Last name" <> ", fulfilled as " <> fulfillmentText r."Fulfillment" <> ", paid " <> r."Payment"."Paid" <> " by " <> caseText r."Payment"."Method"

fulfillmentText :: { "Address" :: String, "Mode" :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ], "Table" :: String, "Time" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] } -> String
fulfillmentText f = match
  { "Dine in": \_ -> "dine in at table " <> f."Table"
  , "Takeaway": \_ -> "takeaway at " <> f."Time"
  , "Delivery": \_ -> "delivery to " <> f."Address" <> awayText f.distance
  } f."Mode"

awayText :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] -> String
awayText = match
  { estimated: \e -> " (" <> show e.km <> " km away)"
  , unknown: const ""
  }

payingLine
  :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
  -> String
payingLine r = "Paying by " <> caseText r."Method"

loadOrder
  :: {}
  -> Aff [ "Order loaded" :: { "Customer" :: { "First name" :: String, "Last name" :: String }
                             , "Fulfillment" :: { "Address" :: String
                                                , "Mode" :: [ "Delivery" :: {}
                                                            , "Dine in" :: {}
                                                            , "Takeaway" :: {}
                                                            ]
                                                , "Table" :: String
                                                , "Time" :: String
                                                , distance :: [ estimated :: { km :: Int
                                                                             , to :: String
                                                                             }
                                                              , unknown :: {}
                                                              ]
                                                }
                             , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
                             , "Kitchen" :: { "Remarks" :: String }
                             , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ]
                                            , "Paid" :: String
                                            , "Total" :: String
                                            }
                             }
         ]
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
    , "Payment": { "Total": "12.30", "Method": ."cash" {}, "Paid": "0.00" }
    , "Kitchen": { "Remarks": "Very spicy, please!" }
    }

submitOrder
  :: { "Customer" :: { "First name" :: String, "Last name" :: String }
     , "Fulfillment" :: { "Address" :: String
                        , "Mode" :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
                        , "Table" :: String
                        , "Time" :: String
                        , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
                        }
     , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
     , "Kitchen" :: { "Remarks" :: String }
     , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
     }
  -> Aff [ "Order submitted" :: { "Short ID" :: String }
         , "Submission failed" :: { "Short ID" :: String, reason :: String }
         ]
submitOrder { "Identifier": { "Short ID": shortId, "Unique ID": orderId }, "Payment": { "Total": total } } = do
  liftEffect $ log $ "submitting order " <> orderId
  delay (Milliseconds 1000.0)
  if total == ""
    then do
      liftEffect $ log "order submission failed"
      pure $ ."Submission failed" { "Short ID": shortId, reason: "missing total" }
    else do
      liftEffect $ log "submitted order"
      pure $ ."Order submitted" { "Short ID": shortId }

submittedLine :: { "Short ID" :: String } -> String
submittedLine { "Short ID": shortId } = "Order " <> shortId <> " submitted"

rejectionLine :: { "Short ID" :: String, reason :: String } -> String
rejectionLine { "Short ID": shortId, reason } = "Order " <> shortId <> " rejected: " <> reason

printReceipt
  :: { "Customer" :: { "First name" :: String, "Last name" :: String }
     , "Fulfillment" :: { "Address" :: String
                        , "Mode" :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
                        , "Table" :: String
                        , "Time" :: String
                        , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
                        }
     , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
     , "Kitchen" :: { "Remarks" :: String }
     , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
     }
  -> Aff [ "Receipt printed" :: { "Short ID" :: String } ]
printReceipt { "Identifier": { "Short ID": shortId, "Unique ID": orderId } } = do
  liftEffect $ log $ "printing receipt for order " <> orderId
  delay (Milliseconds 2000.0)
  liftEffect $ log $ "printed receipt for order " <> orderId
  pure $ ."Receipt printed" { "Short ID": shortId }

receiptLine :: { "Short ID" :: String } -> String
receiptLine { "Short ID": shortId } = "Receipt for order " <> shortId <> " printed"
