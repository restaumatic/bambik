module Demo1Business where

import Prelude

import Data.Invariant.Transformers.Scoped (Scoped, adapter, constructor, field, projection)
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Optics (class ProCocartesian)

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

formal :: forall i. Profunctor i => i (Scoped CustomerFormal) (Scoped CustomerFormal) -> i (Scoped CustomerInformal) (Scoped CustomerInformal)
formal = adapter "formal" toInformal toFormal
  where
    toFormal :: CustomerInformal -> CustomerFormal
    toFormal { firstName: forename, lastName: surname } = { forename, surname }
    toInformal :: CustomerFormal -> CustomerInformal
    toInformal { forename: firstName, surname: lastName } = { firstName, lastName }

--

id = field @"id"
orderedBy = field @"orderedBy"
paid = field @"paid"
firstName = field @"firstName"
lastName = field @"lastName"
forename = field @"forename"
surname =  field @"surname"
fulfillment =  field @"fulfillment"
address =  field @"address"

isDineIn :: forall p a. Profunctor p => p (Scoped Boolean) (Scoped a) → p (Scoped Fulfillment) (Scoped Fulfillment)
isDineIn = adapter "dine-in" (const DineIn) (case _ of
        DineIn -> true
        _ -> false)

isTakeaway :: forall p a. Profunctor p => p (Scoped Boolean) (Scoped a) → p (Scoped Fulfillment) (Scoped Fulfillment)
isTakeaway = adapter "isTakeaway" (const Takeaway) (case _ of
        Takeaway -> true
        _ -> false)

isDelivery :: forall p a. Profunctor p => p (Scoped Boolean) (Scoped a) → p (Scoped Fulfillment) (Scoped Fulfillment)
isDelivery = adapter "isDelivery" (const (Delivery { address: "" })) (case _ of
        Delivery _ -> true
        _ -> false)

delivery :: forall p. ProCocartesian p =>p (Scoped { address :: String } ) (Scoped { address :: String } ) → p (Scoped Fulfillment) (Scoped Fulfillment)
delivery = constructor "isDelivery" Delivery (case _ of
      Delivery c -> Just c
      _ -> Nothing)

shown = projection "show" show
