module Demo1Business
  ( Order
  , CustomerInformal
  , CustomerFormal
  , Fulfillment
  , Address
  , id
  , orderedBy
  , paid
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
  , idCaption
  , paidCaption
  , dineInCaption
  , takeawayCaption
  , deliveryCaption
  , tableCaption
  , timeCaption
  , addressCaption
  , fullfilmentCaption
  , orderCaption
  , writeOrderToConsoleCaption
  , defaultOrder
  ) where
  
import Prelude

import Data.Profunctor.Optics
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)

type Order =
  { id :: String
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

instance Show Fulfillment where
  show (DineIn { table }) = "Dine in at table " <> table
  show (Takeaway { time }) = "Takeaway at " <> time
  show (Delivery { address }) = "Delivery to " <> address

id = field @"id"

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

isDineIn :: Adapter (Maybe Fulfillment) Fulfillment
isDineIn = adapter "isDineIn" (fromMaybe ( DineIn { table: ""})) (case _ of
  d@(DineIn _) -> Just d
  _ -> Nothing)

isTakeaway :: Adapter (Maybe Fulfillment) Fulfillment
isTakeaway = adapter "isTakeaway" (fromMaybe (Takeaway { time: ""})) (case _ of
  t@(Takeaway _) -> Just t
  _ -> Nothing)

isDelivery :: Adapter (Maybe Fulfillment) Fulfillment
isDelivery = adapter "isDelivery" (fromMaybe (Delivery { address: "" })) (case _ of
  d@(Delivery _) -> Just d
  _ -> Nothing)

formal :: Adapter CustomerFormal CustomerInformal
formal = adapter "formal" toInformal toFormal
  where
    toFormal :: CustomerInformal -> CustomerFormal
    toFormal { firstName: forename, lastName: surname } = { forename, surname }
    toInformal :: CustomerFormal -> CustomerInformal
    toInformal { forename: firstName, surname: lastName } = { firstName, lastName }

paymentStatus :: Projection String Order
paymentStatus = projection \order -> if order.paid then "Paid" else "NOT PAID"

fulfillmentData :: Projection String Order
fulfillmentData = projection \order -> show order.fulfillment

writeOrderToConsole :: Order -> Effect Unit
writeOrderToConsole order = log $ show order

firstNameCaption :: Constant String
firstNameCaption = constant "First name"

lastNameCaption :: Constant String
lastNameCaption = constant "Last name"

forenameCaption :: Constant String
forenameCaption = constant "Forename"

surnameCaption :: Constant String
surnameCaption = constant "Surname"

idCaption :: Constant String
idCaption = constant "ID"

paidCaption :: Constant String
paidCaption = constant "Paid"

dineInCaption :: Constant String
dineInCaption = constant "Dine in"

takeawayCaption :: Constant String
takeawayCaption = constant "Takeaway"

deliveryCaption :: Constant String
deliveryCaption = constant "Takeaway"

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

defaultOrder :: Order
defaultOrder =
  { id: "7"
  , orderedBy:
    { firstName: "John"
    , lastName: "Doe"
    }
  , paid: true
  , fulfillment: DineIn { table: "" }
  }

