module ViewModel
  ( Order
  , NameInformal
  , NameFormal
  , Fulfillment
  , Address
  , ShortId
  , UniqueId
  , uniqueId
  , shortId
  , customer
  , paid
  , short
  , unique
  , firstName
  , lastName
  , forename
  , surname
  , fulfillment
  , table
  , time
  , address
  , dineIn
  , takeaway
  , delivery
  , isDineIn
  , isTakeaway
  , isDelivery
  , formal
  , submitOrder
  , orderId
  , total
  , defaultOrder
  ) where
  
import Prelude

import Data.Array (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Optics (Constructor, Iso, Lens', constructor, field, iso, iso', lens')
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)

type Order =
  { uniqueId :: UniqueId
  , shortId :: ShortId
  , customer :: NameInformal
  , paid :: Boolean
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

data Fulfillment
  = DineIn { table :: Table }
  | Takeaway { time :: Time }
  | Delivery { address :: Address }

type Table = String

type Time = String

type Address = String

type OrderId = { short :: String, unique :: String}

-- could be generated by compiler

uniqueId = field @"uniqueId"

shortId = field @"shortId"

customer = field @"customer"

paid = field @"paid"

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

dineIn :: Constructor { table :: Table } Fulfillment
dineIn = constructor "dineIn" DineIn (case _ of
  DineIn c -> Just c
  _ -> Nothing)

takeaway :: Constructor { time :: String } Fulfillment
takeaway = constructor "takeaway" Takeaway (case _ of
  Takeaway c -> Just c
  _ -> Nothing)

delivery :: Constructor { address :: Address } Fulfillment
delivery = constructor "delivery" Delivery (case _ of
  Delivery c -> Just c
  _ -> Nothing)

-- cannot be generated by compiler

isDineIn :: Iso (Maybe Fulfillment) Fulfillment
isDineIn = iso' (case _ of
  d@(DineIn _) -> Just d
  _ -> Nothing) (fromMaybe ( DineIn { table: "1"}))

isTakeaway :: Iso (Maybe Fulfillment) Fulfillment
isTakeaway = iso' (case _ of
  t@(Takeaway _) -> Just t
  _ -> Nothing) (fromMaybe (Takeaway { time: "15:30"}))

isDelivery :: Iso (Maybe Fulfillment) Fulfillment
isDelivery = iso' (case _ of
  d@(Delivery _) -> Just d
  _ -> Nothing) (fromMaybe (Delivery { address: "Mulholland Drive 2001, Los Angeles" }))

formal :: Iso NameFormal NameInformal
formal = iso "formal" toFormal toInformal
  where
    toFormal :: NameInformal -> NameFormal
    toFormal { firstName: forename, lastName: surname } = { forename, surname }
    toInformal :: NameFormal -> NameInformal
    toInformal { forename: firstName, surname: lastName } = { firstName, lastName }

type SerializedOrder = String

submitOrder :: Order -> Aff OrderConfirmation
submitOrder o = do
  let so = serializeOrder o
  liftEffect $ log so
  pure { shortId: o.shortId}
  where
    serializeOrder :: Order -> SerializedOrder
    serializeOrder order = intercalate "|" [order.uniqueId, order.shortId, order.customer.firstName, order.customer.lastName, order.total, if order.paid then "paid" else "not paid", case order.fulfillment of
        (DineIn { table }) -> "dinein|" <> table
        (Takeaway { time }) -> "takeaway|" <> time
        (Delivery { address }) -> "delivery|\"" <> address <> "\""
      ]

orderId :: Lens' OrderId Order
orderId = lens' "orderId" (case _ of
  { uniqueId, shortId} -> { short: shortId, unique: uniqueId }) (\id -> case _ of
    { short, unique } -> id { shortId = short, uniqueId = unique })

--

defaultOrder :: Order
defaultOrder =
  { uniqueId: "71287"
  , shortId: "7"
  , customer:
    { firstName: "David"
    , lastName: "Lynch"
    }
  , total: "12.30"
  , paid: true
  , fulfillment: DineIn { table: "1" }
  }

