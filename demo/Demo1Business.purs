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
  , address
  , delivery
  , isDineIn
  , isTakeaway
  , isDelivery
  , formal
  , print
  , submit
  , defaultOrder
  ) where
  
import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Optics
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

data Fulfillment = DineIn | Takeaway | Delivery { address :: Address }

type Address = String

instance Show Fulfillment where
  show DineIn = "Dine in"
  show Takeaway = "Takeaway"
  show (Delivery { address }) = "Delivery to " <> address

id = field @"id"

orderedBy = field @"orderedBy"

paid = field @"paid"

firstName = field @"firstName"

lastName = field @"lastName"

forename = field @"forename"

surname =  field @"surname"

fulfillment =  field @"fulfillment"

address =  field @"address"

delivery :: Constructor { address :: String } Fulfillment
delivery = constructor "delivery" Delivery (case _ of
  Delivery c -> Just c
  _ -> Nothing)

isDineIn :: Adapter (Maybe Fulfillment) Fulfillment
isDineIn = adapter "isDineIn" (fromMaybe DineIn) (case _ of
  d@DineIn -> Just d
  _ -> Nothing)

isTakeaway :: Adapter (Maybe Fulfillment) Fulfillment
isTakeaway = adapter "isTakeaway" (fromMaybe Takeaway) (case _ of
  t@Takeaway -> Just t
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

print :: forall a. Show a => Projection String a
print = projection "print" show

submit :: Order -> Effect Unit
submit order = log $ show order

defaultOrder :: Order
defaultOrder =
  { id: "61710"
  , orderedBy:
    { firstName: "John"
    , lastName: "Doe"
    }
  , paid: true
  , fulfillment: DineIn
  }

