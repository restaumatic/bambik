module Model where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Widget (WidgetOptics', constructor, field, iso)

type Order =
  { uniqueId :: UniqueId
  , shortId :: ShortId
  , customer :: NameInformal
  , payment :: Maybe Payment
  , fulfillment :: Fulfillment
  , total :: String
  }

type OrderConfirmation =
  { shortId :: ShortId }

type ShortId = String

type UniqueId = String

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

type OrderId = { short :: String, unique :: String}

formal :: WidgetOptics' NameFormal NameInformal
formal = iso "formal" toFormal toInformal
  where
    toFormal :: NameInformal -> NameFormal
    toFormal { firstName, lastName } = { forename: firstName, surname: lastName }
    toInformal :: NameFormal -> NameInformal
    toInformal { forename, surname } = { firstName: forename, lastName: surname }

submitOrder :: Order -> Aff Order
submitOrder order = do
  liftEffect $ log $ "submitting order"
  pure order

defaultOrder :: Order
defaultOrder =
  { uniqueId: "71287"
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

uniqueId = field @"uniqueId"

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
dineIn = constructor "dineIn" DineIn case _ of
  DineIn c -> Just c
  _ -> Nothing

takeaway :: WidgetOptics' { time :: String } Fulfillment
takeaway = constructor "takeaway" Takeaway case _ of
  Takeaway c -> Just c
  _ -> Nothing

delivery :: WidgetOptics' { address :: Address } Fulfillment
delivery = constructor "delivery" Delivery case _ of
  Delivery c -> Just c
  _ -> Nothing
