module Demo1.Model where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (length, null)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Widget (WidgetOptics, WidgetROOptics, WidgetRWOptics, action, constructor, field, iso, lens, projection)

type Order =
  { orderId :: OrderId
  , shortId :: ShortId
  , customer :: NameInformal
  , payment :: Maybe Payment
  , fulfillment :: Fulfillment
  , remarks :: String
  , total :: String
  }

type OrderConfirmation =
  { shortId :: ShortId }

type ShortId = String

type OrderId = String

type NameInformal =
  { firstName :: String
  , lastName :: String
  }

type NameFormal =
  { forename :: String
  , surname :: String
  }

type Payment = { paid :: String }

data Fulfillment
  = DineIn { table :: Table }
  | Takeaway { time :: Time }
  | Delivery { address :: Address }

type Table = String

type Time = String

type Address = String

type AuthorizedOrder =
  { authToken :: AuthToken
  , order :: Order
  }

type AuthToken = String

type OrderSummary = String

-- WidgetRWOptics

orderId :: WidgetRWOptics String Order
orderId = field @"orderId" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

shortId :: WidgetRWOptics String Order
shortId = field @"shortId" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

customer :: WidgetRWOptics NameInformal Order
customer = field @"customer" (\_ _ -> Nothing)

payment :: WidgetRWOptics (Maybe Payment) Order
payment = field @"payment" (\_ _ -> Nothing)

firstName :: WidgetRWOptics String NameInformal
firstName = field @"firstName" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

lastName :: WidgetRWOptics String NameInformal
lastName = field @"lastName" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

forename :: WidgetRWOptics String NameFormal
forename = field @"forename" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

surname :: WidgetRWOptics String NameFormal
surname =  field @"surname" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

fulfillment :: WidgetRWOptics Fulfillment Order
fulfillment =  field @"fulfillment" (\_ _ -> Nothing)

table :: WidgetRWOptics Table { table :: Table }
table =  field @"table" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

time :: WidgetRWOptics Time { time :: Time }
time =  field @"time" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

address :: WidgetRWOptics Address { address :: Address}
address =  field @"address" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

remarks :: WidgetRWOptics String Order
remarks = field @"remarks" (\_ _ -> Nothing)

total :: WidgetRWOptics String Order
total = field @"total" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

paid :: WidgetRWOptics String Payment
paid = field @"paid" (\_ _ -> Nothing)

dineIn :: WidgetRWOptics { table :: Table } Fulfillment
dineIn = constructor "DineIn" DineIn case _ of
  DineIn c -> Just c
  _ -> Nothing

takeaway :: WidgetRWOptics { time :: String } Fulfillment
takeaway = constructor "Takeaway" Takeaway case _ of
  Takeaway c -> Just c
  _ -> Nothing

delivery :: WidgetRWOptics { address :: Address } Fulfillment
delivery = constructor "Delivery" Delivery case _ of
  Delivery c -> Just c
  _ -> Nothing

formal :: WidgetRWOptics NameFormal NameInformal
formal = iso "formal" toFormal toInformal
  where
    toFormal :: NameInformal -> NameFormal
    toFormal { firstName, lastName } = { forename: firstName, surname: lastName }
    toInformal :: NameFormal -> NameInformal
    toInformal { forename, surname } = { firstName: forename, lastName: surname }

-- WidgetROOptics

distance :: WidgetROOptics String Address
distance = projection $ show <<< length

-- other optics


right = constructor "right" Right (case _ of
  Left _ -> Nothing
  Right r -> Just r)

left = constructor "left" Left (case _ of
  Right _ -> Nothing
  Left l -> Just l)


submitOrder :: WidgetOptics Boolean Void AuthorizedOrder (Either String Order)
submitOrder = action \{authToken, order} -> do
  liftEffect $ log $ "submitting order " <> order.orderId <> " with auth token " <> authToken
  delay (Milliseconds 1000.0)
  liftEffect $ log $ "submitted order"
  pure $ Left "Error...."

loadOrder :: WidgetOptics Boolean Void OrderId Order
loadOrder = action \orderId -> do
  liftEffect $ log $ "loading order"
  delay (Milliseconds 1000.0)
  liftEffect $ log $ "loaded order"
  pure
    { orderId
    , shortId: "7"
    , customer:
      { firstName: "David"
      , lastName: "Lynch"
      }
    , total: "12.30"
    , payment: Nothing
    , fulfillment: DineIn { table: "1" }
    , remarks: "I'm very hungry"
    }

authorizarion :: WidgetOptics OrderSummary AuthToken Order AuthorizedOrder
authorizarion = lens "authorization" (\order -> order.total <> " " <> case order.fulfillment of
  DineIn { table } -> "dine-in at table " <> table
  Takeaway { time } -> "takeaway at " <> show time
  Delivery { address } -> "delivery " <> show address) (\order authToken -> { authToken, order })

order :: forall a. OrderId -> WidgetOptics OrderId a Unit a
order id = lens "order" (const id) (\_ a -> a)

authToken :: forall a. WidgetOptics String String a AuthToken
authToken = lens "auth token" (const "") (\_ a -> a)
