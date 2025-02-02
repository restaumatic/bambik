module Demo1.Model
  ( Address
  , AuthToken
  , AuthorizedOrder
  , Fulfillment
  , NameFormal
  , NameInformal
  , Order
  , OrderConfirmation
  , OrderId
  , OrderSummary
  , Payment
  , ShortId
  , Table
  , Time
  , address
  -- , authorizationToken
  , authorizationToken
  , customer
  , delivery
  , dineIn
  , distance
  , firstName
  , forename
  , formal
  , fulfillment
  , lastName
  , loadOrder
  , order
  , orderId
  , paid
  , payment
  , remarks
  , orderSubmissionFailed
  , shortId
  , submitOrder
  , surname
  , table
  , takeaway
  , time
  , total
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (length, null)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Widget (Ctor, Field, WidgetOptics, action, constructor, field, iso, lens, prism, projection)

-- data types

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
  { authorizationToken :: AuthToken
  , order :: Order
  }

type AuthToken = String

type OrderSummary = String

-- Optics

orderId :: Field String Order
orderId = field @"orderId" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

shortId :: Field String Order
shortId = field @"shortId" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

customer :: Field NameInformal Order
customer = field @"customer" (\_ _ -> Nothing)

payment :: Field (Maybe Payment) Order
payment = field @"payment" (\_ _ -> Nothing)

firstName :: Field String NameInformal
firstName = field @"firstName" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

lastName :: Field String NameInformal
lastName = field @"lastName" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

forename :: Field String NameFormal
forename = field @"forename" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

surname :: Field String NameFormal
surname =  field @"surname" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

fulfillment :: Field Fulfillment Order
fulfillment =  field @"fulfillment" (\_ _ -> Nothing)

table :: Field Table { table :: Table }
table =  field @"table" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

time :: Field Time { time :: Time }
time =  field @"time" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

address :: Field Address { address :: Address}
address =  field @"address" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

remarks :: Field String Order
remarks = field @"remarks" (\_ _ -> Nothing)

total :: Field String Order
total = field @"total" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

paid :: Field String Payment
paid = field @"paid" (\_ _ -> Nothing)

dineIn :: Ctor { table :: Table } Fulfillment
dineIn = constructor "DineIn" DineIn case _ of
  DineIn c -> Just c
  _ -> Nothing

takeaway :: Ctor { time :: String } Fulfillment
takeaway = constructor "Takeaway" Takeaway case _ of
  Takeaway c -> Just c
  _ -> Nothing

delivery :: Ctor { address :: Address } Fulfillment
delivery = constructor "Delivery" Delivery case _ of
  Delivery c -> Just c
  _ -> Nothing

formal :: Field NameFormal NameInformal
formal = iso "formal" toFormal toInformal
  where
    toFormal :: NameInformal -> NameFormal
    toFormal { firstName, lastName } = { forename: firstName, surname: lastName }
    toInformal :: NameFormal -> NameInformal
    toInformal { forename, surname } = { firstName: forename, lastName: surname }

distance :: forall t. WidgetOptics String Void Address t
distance = projection $ show <<< length

authorizationToken :: WidgetOptics OrderSummary AuthToken Order AuthorizedOrder
authorizationToken = lens (\order -> order.total <> " " <> case order.fulfillment of
  DineIn { table } -> "dine-in at table " <> table
  Takeaway { time } -> "takeaway at " <> show time
  Delivery { address } -> "delivery " <> show address) (\order authorizationToken -> { authorizationToken, order })

order :: forall a. OrderId -> WidgetOptics OrderId a Unit a
order id = lens (const id) (\_ a -> a)

-- authorizationToken :: forall a. WidgetOptics String String a AuthToken
-- authorizationToken = lens (const "") (\_ a -> a)

orderSubmissionFailed :: WidgetOptics Unit Void Boolean Unit
orderSubmissionFailed = prism absurd case _ of
  false -> Right unit
  true -> Left unit

submitOrder :: WidgetOptics Boolean Void AuthorizedOrder Boolean
submitOrder = action \{authorizationToken, order} -> do
  liftEffect $ log $ "submitting order " <> order.orderId <> " with auth token " <> authorizationToken
  delay (Milliseconds 1000.0)
  liftEffect $ log $ "submitted order"
  -- pure false
  pure true

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