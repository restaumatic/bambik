module OrderFormViewModel (distanceLine, distanceOf, estimateDistance, fulfillmentCase, fulfillmentState, loadOrder, orderLine, payingLine, printReceipt, receiptLine, rejectionLine, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summaryLine, summarySettleTime) where

import Prelude ((<>), ($), (==), (/=), bind, const, discard, pure, show)

import Data.Variant (match)
import Data.Variant.Case (caseText)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Random (randomInt)

estimateDistance
  :: { "Address" :: String
     , "Table" :: String
     , "Time" :: String
     , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
     , selected :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
     }
  -> Aff [ estimated :: { km :: Int, to :: String } ]
estimateDistance { "Address": address } = do
  liftEffect $ log $ "estimating the distance to " <> address
  delay (Milliseconds 700.0)
  km <- liftEffect $ randomInt 1 6
  liftEffect $ log $ "estimated " <> show km <> " km"
  pure $ .estimated { km, to: address }

setDistance
  :: { km :: Int, to :: String }
  -> { "Address" :: String
     , "Table" :: String
     , "Time" :: String
     , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
     , selected :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
     }
  -> { "Address" :: String
     , "Table" :: String
     , "Time" :: String
     , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
     , selected :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
     }
setDistance estimate order = order { distance = .estimated { km: estimate.km, to: estimate.to } }

staleDistanceForgotten
  :: { "Address" :: String
     , "Table" :: String
     , "Time" :: String
     , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
     , selected :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
     }
  -> { "Address" :: String
     , "Table" :: String
     , "Time" :: String
     , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
     , selected :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
     }
staleDistanceForgotten r@{ "Address": address, distance } = match
  { estimated: \e -> if e.to /= address then r { distance = .unknown {} } else r
  , unknown: \_ -> r
  } distance

distanceOf
  :: { "Address" :: String
     , "Table" :: String
     , "Time" :: String
     , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
     , selected :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
     }
  -> [ estimated :: { km :: Int }, unknown :: {} ]
distanceOf { distance } = match { estimated: \e -> .estimated { km: e.km }, unknown: const (.unknown {}) } distance

distanceLine :: { km :: Int } -> String
distanceLine { km } = "Distance " <> show km <> " km"

fulfillmentState
  :: [ "Delivery" :: { "Address" :: String
                     , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
                     }
     , "Dine in" :: { "Table" :: String }
     , "Takeaway" :: { "Time" :: String }
     ]
  -> { "Address" :: String
     , "Table" :: String
     , "Time" :: String
     , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
     , selected :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
     }
fulfillmentState = match
  { "Dine in": \r -> { selected: ."Dine in" {}, "Table": r."Table", "Time": "12:00", "Address": "", distance: .unknown {} }
  , "Takeaway": \r -> { selected: ."Takeaway" {}, "Table": "1", "Time": r."Time", "Address": "", distance: .unknown {} }
  , "Delivery": \r -> { selected: ."Delivery" {}, "Table": "1", "Time": "12:00", "Address": r."Address", distance: r.distance }
  }

fulfillmentCase
  :: { "Address" :: String
     , "Table" :: String
     , "Time" :: String
     , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
     , selected :: [ "Delivery" :: {}, "Dine in" :: {}, "Takeaway" :: {} ]
     }
  -> [ "Delivery" :: { "Address" :: String
                     , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
                     }
     , "Dine in" :: { "Table" :: String }
     , "Takeaway" :: { "Time" :: String }
     ]
fulfillmentCase { selected, "Table": table, "Time": time, "Address": address, distance } = match
  { "Dine in": \_ -> ."Dine in" { "Table": table }
  , "Takeaway": \_ -> ."Takeaway" { "Time": time }
  , "Delivery": \_ -> ."Delivery" { "Address": address, distance }
  } selected

orderLine
  :: { "Customer" :: { "First name" :: String, "Last name" :: String }
     , "Fulfillment" :: { "Mode" :: [ "Delivery" :: { "Address" :: String
                                                    , distance :: [ estimated :: { km :: Int
                                                                                 , to :: String
                                                                                 }
                                                                  , unknown :: {}
                                                                  ]
                                                    }
                                    , "Dine in" :: { "Table" :: String }
                                    , "Takeaway" :: { "Time" :: String }
                                    ]
                        }
     , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
     , "Kitchen" :: { "Remarks" :: String }
     , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
     }
  -> String
orderLine r = "Order " <> r."Identifier"."Short ID"

summaryLine
  :: { "Customer" :: { "First name" :: String, "Last name" :: String }
     , "Fulfillment" :: { "Mode" :: [ "Delivery" :: { "Address" :: String
                                                    , distance :: [ estimated :: { km :: Int
                                                                                 , to :: String
                                                                                 }
                                                                  , unknown :: {}
                                                                  ]
                                                    }
                                    , "Dine in" :: { "Table" :: String }
                                    , "Takeaway" :: { "Time" :: String }
                                    ]
                        }
     , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
     , "Kitchen" :: { "Remarks" :: String }
     , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
     }
  -> String
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

payingLine
  :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
  -> String
payingLine r = "Paying by " <> caseText r."Method"

loadOrder
  :: {}
  -> Aff [ "Order loaded" :: { "Customer" :: { "First name" :: String, "Last name" :: String }
                             , "Fulfillment" :: { "Mode" :: [ "Delivery" :: { "Address" :: String
                                                                            , distance :: [ estimated :: { km :: Int
                                                                                                         , to :: String
                                                                                                         }
                                                                                          , unknown :: {}
                                                                                          ]
                                                                            }
                                                            , "Dine in" :: { "Table" :: String }
                                                            , "Takeaway" :: { "Time" :: String }
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
    , "Fulfillment": { "Mode": ."Takeaway" { "Time": "8:30" } }
    , "Payment": { "Total": "12.30", "Method": ."cash" {}, "Paid": "0.00" }
    , "Kitchen": { "Remarks": "Very spicy, please!" }
    }

submitOrder
  :: { "Customer" :: { "First name" :: String, "Last name" :: String }
     , "Fulfillment" :: { "Mode" :: [ "Delivery" :: { "Address" :: String
                                                    , distance :: [ estimated :: { km :: Int
                                                                                 , to :: String
                                                                                 }
                                                                  , unknown :: {}
                                                                  ]
                                                    }
                                    , "Dine in" :: { "Table" :: String }
                                    , "Takeaway" :: { "Time" :: String }
                                    ]
                        }
     , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
     , "Kitchen" :: { "Remarks" :: String }
     , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
     }
  -> Aff [ "Order submitted" :: { "Short ID" :: String }
         , "Receipt printed" :: { "Short ID" :: String }
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
     , "Fulfillment" :: { "Mode" :: [ "Delivery" :: { "Address" :: String
                                                    , distance :: [ estimated :: { km :: Int
                                                                                 , to :: String
                                                                                 }
                                                                  , unknown :: {}
                                                                  ]
                                                    }
                                    , "Dine in" :: { "Table" :: String }
                                    , "Takeaway" :: { "Time" :: String }
                                    ]
                        }
     , "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
     , "Kitchen" :: { "Remarks" :: String }
     , "Payment" :: { "Method" :: [ card :: {}, cash :: {} ], "Paid" :: String, "Total" :: String }
     }
  -> Aff [ "Order submitted" :: { "Short ID" :: String }
         , "Receipt printed" :: { "Short ID" :: String }
         , "Submission failed" :: { "Short ID" :: String, reason :: String }
         ]
printReceipt { "Identifier": { "Short ID": shortId, "Unique ID": orderId } } = do
  liftEffect $ log $ "printing receipt for order " <> orderId
  delay (Milliseconds 2000.0)
  liftEffect $ log $ "printed receipt for order " <> orderId
  pure $ ."Receipt printed" { "Short ID": shortId }

receiptLine :: { "Short ID" :: String } -> String
receiptLine { "Short ID": shortId } = "Receipt for order " <> shortId <> " printed"

summarySettleTime :: { ms :: Number }
summarySettleTime = { ms: 300.0 }
