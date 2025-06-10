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
  , normal
  , orderId
  , orderSubmission
  , orderSubmissionFailed
  , paid
  , payment
  , paymentMethod
  , priority
  , receiptPrint
  , remarks
  , shortId
  , summary
  , surname
  , table
  , takeaway
  , time
  , total
  )
  where

import Prelude

import Data.Default (class Default)
import Data.Either (Either(..))
import Data.Lens (Iso, Lens, Prism, iso, lens, prism)
import Data.Lens.Extra.Commons (constructor, field, projection)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)

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

instance Default PaymentMethod where
  default = Cash

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

-- optics

summary :: forall t. Iso OrderSummary t String Void
summary = projection (_.summary)

priority :: Lens Order Order Priority Priority
priority = field @"priority"

orderId :: Lens Order Order String String
orderId = field @"orderId"

shortId :: Lens Order Order String String 
shortId = field @"shortId"

customer :: Lens Order Order NameInformal NameInformal 
customer = field @"customer"

payment :: Lens Order Order (Maybe Payment) (Maybe Payment)
payment = field @"payment"

firstName :: Lens NameInformal NameInformal String String
firstName = field @"firstName"

lastName :: Lens NameInformal NameInformal String String
lastName = field @"lastName"

forename :: Lens NameFormal NameFormal String String
forename = field @"forename"

surname :: Lens NameFormal NameFormal String String
surname =  field @"surname"

fulfillment :: Lens Order Order Fulfillment Fulfillment 
fulfillment =  field @"fulfillment"

table :: Lens { table :: Table } { table :: Table } Table Table
table =  field @"table"

time :: Lens { time :: Time } { time :: Time } Time Time
time =  field @"time"

address :: Lens { address :: Address} { address :: Address} Address Address
address =  field @"address"

remarks :: Lens Order Order String String
remarks = field @"remarks"

total :: Lens Order Order String String
total = field @"total"

paid :: Lens Payment Payment String String
paid = field @"paid"

dineIn :: Iso Fulfillment Fulfillment (Maybe { table :: Table }) { table :: Table }
dineIn = constructor DineIn case _ of
  DineIn c -> Just c
  _ -> Nothing

takeaway :: Iso Fulfillment Fulfillment (Maybe { time :: Time }) { time :: Time }
takeaway = constructor Takeaway case _ of
  Takeaway c -> Just c
  _ -> Nothing

delivery :: Iso Fulfillment Fulfillment (Maybe { address :: Address }) { address :: Address }
delivery = constructor Delivery case _ of
  Delivery c -> Just c
  _ -> Nothing

high :: Iso Priority Priority (Maybe Unit) Unit
high = constructor (const High) case _ of
  High -> Just unit
  _ -> Nothing

normal :: Iso Priority Priority (Maybe Unit) Unit
normal = constructor (const Normal) case _ of
  Normal -> Just unit
  _ -> Nothing

low :: Iso Priority Priority (Maybe Unit) Unit
low = constructor (const Low) case _ of
  Low -> Just unit
  _ -> Nothing

formal :: Iso NameInformal NameInformal NameFormal NameFormal
formal = iso toFormal toInformal
  where
    toFormal :: NameInformal -> NameFormal
    toFormal { firstName, lastName } = { forename: firstName, surname: lastName }
    toInformal :: NameFormal -> NameInformal
    toInformal { forename, surname } = { firstName: forename, lastName: surname }

distance :: forall t. Iso Address t String Void
distance = projection $ show <<< length

authorization :: Lens Order AuthorizedOrder OrderSummary AuthToken
authorization = lens (\order -> { summary: order.total <> " " <> case order.fulfillment of
  DineIn { table } -> "dine-in at table " <> table
  Takeaway { time } -> "takeaway at " <> show time
  Delivery { address } -> "delivery " <> show address }
  ) (\order authorization -> { authorization, order })

orderSubmissionFailed :: Prism Boolean Unit Unit Void
orderSubmissionFailed = prism absurd case _ of
  false -> Right unit
  true -> Left unit

paymentMethod :: Lens Payment Payment PaymentMethod PaymentMethod
paymentMethod = field @"method"

cash :: Iso PaymentMethod PaymentMethod (Maybe Unit) Unit
cash = constructor (const Cash) case _ of
  Cash -> Just unit
  _ -> Nothing

card :: Iso PaymentMethod PaymentMethod (Maybe Unit) Unit
card = constructor (const Card) case _ of
  Card -> Just unit
  _ -> Nothing

-- asynchronous actions

orderSubmission :: AuthorizedOrder -> Aff Boolean
orderSubmission {authorization, order} = do
  liftEffect $ log $ "submitting order " <> order.orderId <> " with auth token " <> authorization
  delay (Milliseconds 1000.0)
  liftEffect $ log $ "submitted order"
  pure true

loadOrder :: Unit -> Aff Order
loadOrder _ = do
  liftEffect $ log $ "loading order"
  delay (Milliseconds 1000.0)
  liftEffect $ log $ "loaded order"
  pure
    { orderId: "4617821"
    , shortId: "7"
    , customer:
      { firstName: "John"
      , lastName: "Doe"
      }
    , total: "12.30"
    , payment: Nothing
    , fulfillment: Takeaway { time: "8:30" }
    , remarks: "Very spicy, please!"
    , priority: Normal
    }

receiptPrint :: Order -> Aff Order
receiptPrint order = do
  liftEffect $ log $ "printing receipt for order " <> order.orderId
  delay (Milliseconds 2000.0)
  liftEffect $ log $ "printed receipt for order " <> order.orderId
  pure order
