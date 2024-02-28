module Model where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Widget (WidgetOptics', WidgetOptics, action, constructor, field, iso)

type Order =
  { orderId :: OrderId
  , shortId :: ShortId
  , customer :: NameInformal
  , payment :: Maybe Payment
  , fulfillment :: Fulfillment
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

formal :: WidgetOptics' NameFormal NameInformal
formal = iso "formal" toFormal toInformal
  where
    toFormal :: NameInformal -> NameFormal
    toFormal { firstName, lastName } = { forename: firstName, surname: lastName }
    toInformal :: NameFormal -> NameInformal
    toInformal { forename, surname } = { firstName: forename, lastName: surname }

type AuthToken = String

submitOrder :: forall a . WidgetOptics Boolean a { authToken :: String , order :: Order } Order
submitOrder = action \{authToken, order} -> do
  liftEffect $ log $ "submitting order " <> order.orderId <> " with auth token " <> authToken
  delay (Milliseconds 1000.0)
  liftEffect $ log $ "submitted order"
  pure order

loadOrder :: forall a . WidgetOptics Boolean a OrderId Order
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
    }

-- could be generated by compiler

derive instance Generic Fulfillment _

instance Show Fulfillment where
  show = genericShow

orderId = field @"orderId"

shortId = field @"shortId"

customer = field @"customer"

payment = field @"payment"

firstName = field @"firstName"

lastName = field @"lastName"

forename = field @"forename"

surname =  field @"surname"

fulfillment =  field @"fulfillment"

table =  field @"table"

time =  field @"time"

address =  field @"address"

unique = field @"unique"

short = field @"short"

total = field @"total"

paid = field @"paid"

dineIn :: WidgetOptics' { table :: Table } Fulfillment
dineIn = constructor "DineIn" DineIn case _ of
  DineIn c -> Just c
  _ -> Nothing

takeaway :: WidgetOptics' { time :: String } Fulfillment
takeaway = constructor "Takeaway" Takeaway case _ of
  Takeaway c -> Just c
  _ -> Nothing

delivery :: WidgetOptics' { address :: Address } Fulfillment
delivery = constructor "Delivery" Delivery case _ of
  Delivery c -> Just c
  _ -> Nothing
