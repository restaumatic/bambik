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
  , paymentStatus
  , submitOrder
  , orderId
  , orderIdText
  , orderTitle
  , serializeOrder
  , orderSummary
  , total
  , defaultOrder
  ) where
  
import Prelude

import Data.Array (intercalate)
import Data.Profunctor.Optics
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)

type Order =
  { uniqueId :: UniqueId
  , shortId :: ShortId
  , customer :: NameInformal
  , paid :: Boolean
  , fulfillment :: Fulfillment
  , total :: String
  }

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

paymentStatus :: Projection String Boolean
paymentStatus = projection if _ then "paid" else "NOT PAID"

type SerializedOrder = String

submitOrder :: SerializedOrder -> Effect Unit
submitOrder = log

serializeOrder :: forall p. Profunctor p => Category p => p Order SerializedOrder
serializeOrder = arr \order -> intercalate "|" [order.uniqueId, order.shortId, order.customer.firstName, order.customer.lastName, order.total, if order.paid then "paid" else "not paid", case order.fulfillment of
    (DineIn { table }) -> "dinein|" <> table
    (Takeaway { time }) -> "takeaway|" <> time
    (Delivery { address }) -> "delivery|\"" <> address <> "\""
  ]

orderId :: Lens' OrderId Order
orderId = lens' "orderId" (case _ of
  { uniqueId, shortId} -> { short: shortId, unique: uniqueId }) (\id -> case _ of
    { short, unique } -> id { shortId = short, uniqueId = unique })

orderIdText :: forall p. ChProfunctor p => Strong p => Choice p => ProfunctorPlus p => (forall a. p String a) -> p OrderId OrderId
orderIdText text = text # short ^ text # fixed " (" ^ text # unique ^ text # fixed ")"

orderTitle :: forall p. ChProfunctor p => ProfunctorPlus p => Strong p => p String String -> p Order Order
orderTitle text = text # fixed "Order " ^ text # shortId

orderSummary :: forall p. ChProfunctor p => ProfunctorPlus p => Strong p => Choice p => p String String -> p Order Order
orderSummary text =
  ( text # fixed "Summary: Order "
  ^ text # shortId
  ^ text # fixed " (uniquely "
  ^ text # uniqueId
  ^ text # fixed ") for "
  ^ ( text # firstName
    ^ text # fixed " "
    ^ text # lastName
      ^ ( text # fixed " (formally "
        ^ text # surname
        ^ text # fixed " "
        ^ text # forename
        ^ text # fixed ")" ) # formal ) # customer
  ^ text # fixed ", fulfilled as "
  ^ fulfillmentData text # fulfillment )
    where
      fulfillmentData :: forall p. ChProfunctor p => Strong p => Choice p => ProfunctorPlus p => p String String -> p Fulfillment Fulfillment
      fulfillmentData text =
        ( (text # fixed "dine in at table " ^ text) # table # dineIn
        ^ (text # fixed "takeaway at " ^ text) # time # takeaway
        ^ (text # fixed "delivery to " ^ text) # address # delivery )

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

