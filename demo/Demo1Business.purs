module Demo1Business where

import Prelude hiding (div)

import Data.Invariant.Transformers.Scoped (Scoped, adapter)
import Data.Profunctor (class Profunctor)

type Order =
  { id :: String
  , customer :: CustomerInformal
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
