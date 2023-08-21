module Demo1Business
  ( Order
  , CustomerInformal
  , CustomerFormal
  , Fulfillment
  , Address
  , uniqueId
  , shortId
  , orderedBy
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
  , writeOrderToConsole
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
  , writeOrderToConsoleCaption
  , defaultOrder
  ) where
  
import Prelude

import Data.Profunctor.Optics
import Data.Array (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)

type Order =
  { uniqueId :: String
  , shortId :: String
  , orderedBy :: CustomerInformal
  , paid :: Boolean
  , fulfillment :: Fulfillment
  }
type CustomerInformal =
  { firstName :: String
  , lastName :: String
  }

type CustomerFormal =
  { forename :: String
  , surname :: String
  }

data Fulfillment = DineIn { table :: Table } | Takeaway { time :: Time } | Delivery { address :: Address }

type Table = String

type Time = String

type Address = String

type OrderId = { short :: String, unique :: String}

uniqueId = field @"uniqueId"

shortId = field @"shortId"

orderedBy = field @"orderedBy"

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

isDineIn :: Iso (Maybe Fulfillment) Fulfillment
isDineIn = iso "isDineIn" (fromMaybe ( DineIn { table: ""})) (case _ of
  d@(DineIn _) -> Just d
  _ -> Nothing)

isTakeaway :: Iso (Maybe Fulfillment) Fulfillment
isTakeaway = iso "isTakeaway" (fromMaybe (Takeaway { time: ""})) (case _ of
  t@(Takeaway _) -> Just t
  _ -> Nothing)

isDelivery :: Iso (Maybe Fulfillment) Fulfillment
isDelivery = iso "isDelivery" (fromMaybe (Delivery { address: "" })) (case _ of
  d@(Delivery _) -> Just d
  _ -> Nothing)

formal :: Iso CustomerFormal CustomerInformal
formal = iso "formal" toInformal toFormal
  where
    toFormal :: CustomerInformal -> CustomerFormal
    toFormal { firstName: forename, lastName: surname } = { forename, surname }
    toInformal :: CustomerFormal -> CustomerInformal
    toInformal { forename: firstName, surname: lastName } = { firstName, lastName }

paymentStatus :: Projection String Boolean
paymentStatus = projection if _ then "Paid" else "NOT PAID"

fulfillmentData :: Projection String Fulfillment
fulfillmentData = projection case _ of
  (DineIn { table }) -> "Dine in at table " <> table
  (Takeaway { time }) -> "Takeaway at " <> time
  (Delivery { address }) -> "Delivery to " <> address

writeOrderToConsole :: Order -> Effect Unit
writeOrderToConsole = log <<< case _ of
  { uniqueId, orderedBy, paid, fulfillment } -> intercalate ", " [uniqueId, orderedBy.firstName, orderedBy.lastName, if paid then "paid" else "not paid", case fulfillment of
    (DineIn { table }) -> "Dine in at table " <> table
    (Takeaway { time }) -> "Takeaway at " <> time
    (Delivery { address }) -> "Delivery to " <> address
  ]

firstNameCaption :: Constant String
firstNameCaption = constant "First name"

lastNameCaption :: Constant String
lastNameCaption = constant "Last name"

forenameCaption :: Constant String
forenameCaption = constant "Forename"

surnameCaption :: Constant String
surnameCaption = constant "Surname"

shortCaption :: Constant String
shortCaption = constant "Short"

uniqueCaption :: Constant String
uniqueCaption = constant "Unique"

shortIdCaption :: Constant String
shortIdCaption = constant "Short ID"

uniqueIdCaption :: Constant String
uniqueIdCaption = constant "Unique ID"

paidCaption :: Constant String
paidCaption = constant "Paid"

dineInCaption :: Constant String
dineInCaption = constant "Dine in"

takeawayCaption :: Constant String
takeawayCaption = constant "Takeaway"

deliveryCaption :: Constant String
deliveryCaption = constant "Delivery"

tableCaption :: Constant String
tableCaption = constant "Table"

timeCaption :: Constant String
timeCaption = constant "Time"

addressCaption :: Constant String
addressCaption = constant "Address"

fullfilmentCaption :: Constant String
fullfilmentCaption = constant "Fullfilment"

orderCaption :: Constant String
orderCaption = constant "Order"

writeOrderToConsoleCaption :: Constant String
writeOrderToConsoleCaption = constant "Write order to console"

orderId :: Lens' OrderId Order
orderId = lens' "orderId" (case _ of
  { uniqueId, shortId} -> { short: shortId, unique: uniqueId }) (\id -> case _ of
    { short, unique } -> id { shortId = short, uniqueId = unique })

orderIdCaption :: Constant String
orderIdCaption = constant "Order ID"

orderIdText :: Projection String OrderId
orderIdText = projection case _ of
  { short, unique } -> short <> " (" <> unique <> ")"

defaultOrder :: Order
defaultOrder =
  { uniqueId: "71287"
  , shortId: "7"
  , orderedBy:
    { firstName: "John"
    , lastName: "Doe"
    }
  , paid: true
  , fulfillment: DineIn { table: "1" }
  }

