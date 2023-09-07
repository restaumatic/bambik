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
  , fulfillmentData
  , submitOrder
  , firstNameCaption
  , lastNameCaption
  , forenameCaption
  , surnameCaption
  , shortCaption
  , uniqueCaption
  , shortIdCaption
  , uniqueIdCaption
  , paidCaption
  , dineInCaption
  , takeawayCaption
  , deliveryCaption
  , tableCaption
  , timeCaption
  , addressCaption
  , fullfilmentCaption
  , orderCaption
  , orderId
  , orderIdCaption
  , orderIdText
  , customerCaption
  , submitOrderCaption
  , orderSubmittedCaption
  , informalCaption
  , formalCaption
  , idCaption
  , orderTitle
  , areYouSureText
  , serializeOrder
  , orderSummary
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

fulfillmentData :: forall p. ChProfunctor p => Strong p => Choice p => ProfunctorPlus p => p String String -> p Fulfillment Fulfillment
fulfillmentData text =
  ( (text # fixed "dine in at table " ^ text) # table # dineIn
  ^ (text # fixed "takeaway at " ^ text) # time # takeaway
  ^ (text # fixed "delivery to " ^ text) # address # delivery )

type SerializedOrder = String

submitOrder :: SerializedOrder -> Effect Unit
submitOrder = log

serializeOrder :: forall p. Profunctor p => Category p => p Order SerializedOrder
serializeOrder = arr \order -> "submitted order: " <> intercalate "|" [order.uniqueId, order.shortId, order.customer.firstName, order.customer.lastName, if order.paid then "paid" else "not paid", case order.fulfillment of
    (DineIn { table }) -> "dinein|" <> table
    (Takeaway { time }) -> "takeaway|" <> time
    (Delivery { address }) -> "delivery|\"" <> address <> "\""
  ]

firstNameCaption :: forall p a. ChProfunctor p => p String String -> p a a
firstNameCaption text = text # fixed "First name"

lastNameCaption :: forall p a. ChProfunctor p => p String String -> p a a
lastNameCaption text = text # fixed "Last name"

forenameCaption :: forall p a. ChProfunctor p => p String String -> p a a
forenameCaption text = text # fixed "Forename"

surnameCaption :: forall p a. ChProfunctor p => p String String -> p a a
surnameCaption text = text # fixed "Surname"

shortCaption :: forall p a. ChProfunctor p => p String String -> p a a
shortCaption text = text # fixed "Short"

uniqueCaption :: forall p a. ChProfunctor p => p String String -> p a a
uniqueCaption text = text # fixed "Unique"

idCaption :: forall p a. ChProfunctor p => p String String -> p a a
idCaption text = text # fixed "Identifier"

shortIdCaption :: forall p a. ChProfunctor p => p String String -> p a a
shortIdCaption text = text # fixed "Short ID"

uniqueIdCaption :: forall p a. ChProfunctor p => p String String -> p a a
uniqueIdCaption text = text # fixed "Unique ID"

paidCaption :: forall p a. ChProfunctor p => p String String -> p a a
paidCaption text = text # fixed "Paid"

dineInCaption :: forall p a. ChProfunctor p => p String String -> p a a
dineInCaption text = text # fixed "Dine in"

takeawayCaption :: forall p a. ChProfunctor p => p String String -> p a a
takeawayCaption text = text # fixed "Takeaway"

deliveryCaption :: forall p a. ChProfunctor p => p String String -> p a a
deliveryCaption text = text # fixed "Delivery"

tableCaption :: forall p a. ChProfunctor p => p String String -> p a a
tableCaption text = text # fixed "Table"

timeCaption :: forall p a b. ChProfunctor p => p String String -> p a b
timeCaption text = text # fixed "Time"

addressCaption :: forall p a. ChProfunctor p => p String String -> p a a
addressCaption text = text # fixed "Address"

fullfilmentCaption :: forall p a. ChProfunctor p => p String String -> p a a
fullfilmentCaption text = text # fixed "Fullfilment"

orderCaption :: forall p a. ChProfunctor p => p String String -> p a a
orderCaption text = text # fixed "Order"

submitOrderCaption :: forall p. ChProfunctor p => ProfunctorPlus p => Strong p => p String String -> p Order Order
submitOrderCaption text =
  ( text # fixed "Submit order "
  ^ text # shortId )

orderSubmittedCaption :: forall p. ChProfunctor p => ProfunctorPlus p => Strong p => p String String -> p SerializedOrder SerializedOrder
orderSubmittedCaption text = text
  -- ( text # fixed "Order "
  -- ^ text # shortId
  -- ^ text # fixed " submitted" )

orderId :: Lens' OrderId Order
orderId = lens' "orderId" (case _ of
  { uniqueId, shortId} -> { short: shortId, unique: uniqueId }) (\id -> case _ of
    { short, unique } -> id { shortId = short, uniqueId = unique })

orderIdCaption :: forall p a. ChProfunctor p => p String String -> p a a
orderIdCaption text = text # fixed "Order ID"

customerCaption :: forall p a. ChProfunctor p => p String String -> p a a
customerCaption text = text # fixed "Customer"

informalCaption :: forall p a. ChProfunctor p => p String String -> p a a
informalCaption text = text # fixed "Informal"

formalCaption :: forall p a. ChProfunctor p => p String String -> p a a
formalCaption text = text # fixed "Formal"

orderIdText :: forall p. ChProfunctor p => Strong p => Choice p => ProfunctorPlus p => (forall a. p String a) -> p OrderId OrderId
orderIdText text = text # short ^ text # fixed " (" ^ text # unique ^ text # fixed ")"

orderTitle :: forall p. ChProfunctor p => ProfunctorPlus p => Strong p => p String String -> p Order Order
orderTitle text = text # fixed "Order " ^ text # shortId

areYouSureText :: forall p a. ChProfunctor p => p String String -> p a a
areYouSureText text = text # fixed "Are you sure?"

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

--

defaultOrder :: Order
defaultOrder =
  { uniqueId: "71287"
  , shortId: "7"
  , customer:
    { firstName: "David"
    , lastName: "Lynch"
    }
  , paid: true
  , fulfillment: DineIn { table: "1" }
  }

