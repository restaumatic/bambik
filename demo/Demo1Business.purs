module Demo1Business where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor.Optics

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
delivery = constructor "isDelivery" Delivery (case _ of
      Delivery c -> Just c
      _ -> Nothing)

isDineIn :: forall a. Adapter Boolean a Fulfillment Fulfillment
isDineIn = adapter "dine-in" (const DineIn) (case _ of
        DineIn -> true
        _ -> false)

isTakeaway :: forall a. Adapter Boolean a Fulfillment Fulfillment
isTakeaway = adapter "isTakeaway" (const Takeaway) (case _ of
        Takeaway -> true
        _ -> false)

isDelivery :: forall a. Adapter Boolean a Fulfillment Fulfillment
isDelivery = adapter "isDelivery" (const (Delivery { address: "" })) (case _ of
        Delivery _ -> true
        _ -> false)

formal :: Adapter CustomerFormal CustomerFormal CustomerInformal CustomerInformal
formal = adapter "formal" toInformal toFormal
  where
    toFormal :: CustomerInformal -> CustomerFormal
    toFormal { firstName: forename, lastName: surname } = { forename, surname }
    toInformal :: CustomerFormal -> CustomerInformal
    toInformal { forename: firstName, surname: lastName } = { firstName, lastName }

-- TODO move to more general module?
shown :: forall s. Show s => Projection String s
shown = projection "show" show
