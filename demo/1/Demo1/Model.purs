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
  , PaymentMethod(..)
  , Priority(..)
  , ShortId
  , Table
  , Time
  , address
  , authorization
  , card
  , cash
  , customer
  , delivery
  , dineIn
  , distance
  , firstName
  , forename
  , formal
  , fulfillment
  , high
  , lastName
  , loadOrder
  , low
  , missing
  , normal
  , order
  , orderId
  , orderSubmissionFailed
  , paid
  , payment
  , paymentMethod
  , priority
  , priorityAssignment
  , remarks
  , shortId
  , submitOrder
  , summary
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
import UI (Field, UIO, UIOptics, Ctor, action, constructor, field, iso, lens, prism, projection)

-- data types

type Order =
  { orderId :: OrderId
  , shortId :: ShortId
  , customer :: NameInformal
  , payment :: Maybe Payment
  , fulfillment :: Fulfillment
  , remarks :: String
  , total :: String
  , priority :: Priority
  }

data Priority = Low | Normal | High

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

type Payment = { method :: PaymentMethod, paid :: String }

data PaymentMethod = Cash | Card

data Fulfillment
  = DineIn { table :: Table }
  | Takeaway { time :: Time }
  | Delivery { address :: Address }

type Table = String

type Time = String

type Address = String

type AuthorizedOrder =
  { authorization :: AuthToken
  , order :: Order
  }

type AuthToken = String

type OrderSummary = { summary :: String }

-- Optics

summary :: forall t. UIOptics String Void OrderSummary t
summary = projection (_.summary)

priority :: Field Priority Order
priority = field @"priority" (\str _ -> Nothing)

orderId :: Field String Order
orderId = field @"orderId" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

shortId :: Field String Order
shortId = field @"shortId" (\str _ -> if null str then Just "Cannot be empty" else Nothing)

customer :: Field NameInformal Order
customer = field @"customer" (\_ _ -> Nothing)

-- payment :: Field (Maybe Payment) Order
payment :: UIO Order Order (Maybe Payment) (Maybe Payment)
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

high :: Ctor Unit Priority
high = constructor "High" (const High) case _ of
  High -> Just unit
  _ -> Nothing

normal :: Ctor Unit Priority
normal = constructor "Normal" (const Normal) case _ of
  Normal -> Just unit
  _ -> Nothing

low :: Ctor Unit Priority
low = constructor "Low" (const Low) case _ of
  Low -> Just unit
  _ -> Nothing

formal :: Field NameFormal NameInformal
formal = iso "formal" toFormal toInformal
  where
    toFormal :: NameInformal -> NameFormal
    toFormal { firstName, lastName } = { forename: firstName, surname: lastName }
    toInformal :: NameFormal -> NameInformal
    toInformal { forename, surname } = { firstName: forename, lastName: surname }

distance :: forall t. UIOptics String Void Address t
distance = projection $ show <<< length

authorization :: UIOptics OrderSummary AuthToken Order AuthorizedOrder
authorization = lens (\order -> { summary: order.total <> " " <> case order.fulfillment of
  DineIn { table } -> "dine-in at table " <> table
  Takeaway { time } -> "takeaway at " <> show time
  Delivery { address } -> "delivery " <> show address }
  ) (\order authorization -> { authorization, order })

order :: forall a. OrderId -> UIOptics OrderId a Unit a
order id = lens (const id) (\_ a -> a)

-- authorization :: forall a. UIOptics String String a AuthToken
-- authorization = lens (const "") (\_ a -> a)

orderSubmissionFailed :: UIOptics Unit Void Boolean Unit
orderSubmissionFailed = prism absurd case _ of
  false -> Right unit
  true -> Left unit

submitOrder :: UIOptics Boolean Void AuthorizedOrder Boolean
submitOrder = action \{authorization, order} -> do
  liftEffect $ log $ "submitting order " <> order.orderId <> " with auth token " <> authorization
  delay (Milliseconds 1000.0)
  liftEffect $ log $ "submitted order"
  -- pure false
  pure true

loadOrder :: UIOptics Boolean Void OrderId Order
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
    , priority: Normal
    }

priorityAssignment :: UIOptics Unit Priority Order Order
priorityAssignment = lens (\order -> unit) (\order priority -> order { priority = priority })

paymentMethod :: UIO Payment Payment PaymentMethod PaymentMethod
paymentMethod = field @"method" (\_ _ -> Nothing)

cash :: Ctor Unit PaymentMethod
cash = constructor "Cash" (const Cash) case _ of
  Cash -> Just unit
  _ -> Nothing

card :: Ctor Unit PaymentMethod
card = constructor "Card" (const Card) case _ of
  Card -> Just unit
  _ -> Nothing

-- TODO: move to commons?
missing :: forall a. UIO (Maybe a) (Maybe a) Unit a
missing = prism Just case _ of
  Just a -> Left (Just a)
  Nothing -> Right unit
