module Test.OrderOptics
  ( Addition
  , Customer
  , Fulfillment
  , Hour
  , Item
  , Note
  , Order
  , PaymentMethod
  , Place
  , Product
  , address
  , at
  , card
  , cash
  , city
  , coords
  , customer
  , delivery
  , dineIn
  , fulfillment
  , hasNote
  , id
  , isDelivery
  , isDineIn
  , isTakeaway
  , items
  , lat
  , long
  , note
  , paymentMethod
  , paymentMethod'
  , product
  , qty
  , street
  , streetNumber
  , takeaway
  , to
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Invariant.Optics (invAffineTraversal, invLens)
import Data.Invariant.Transformers.Scoped (invConstructor, invField)
import Data.Maybe (Maybe(..), isJust)

type Order =
  { id :: Maybe String
  , fulfillment :: Fulfillment
  , items :: Array Item
  , paymentMethod :: PaymentMethod
  , customer :: Customer
  , note :: Maybe Note
  }

type Customer = String

type Note = String

data PaymentMethod = Cash | Card

derive instance Eq PaymentMethod

data Fulfillment
  = DineIn
  | Takeaway
    { at :: Hour}
  | Delivery
    { to :: Place
    , at :: Hour
    }

type Item =
  { product :: Product
  , qty :: Int
  , addition :: Maybe Addition
  }

type Hour = String

data Place
  = Coords
    { long :: String
    , lat :: String
    }
  | Address
    { city :: String
    , street :: String
    , streetNumber :: String}

derive instance Eq Place

type Product = String

type Addition = String

-- optics
-- id :: forall a r . InvLens a { id ∷ a | r }
id = invField @"id"

-- items :: forall a r . InvLens a { items ∷ a | r }
items = invField @"items"

-- delivery :: InvPrism { at ∷ String , to ∷ Place } Fulfillment
delivery = invConstructor "Delivery" Delivery $ case _ of
  Delivery d -> Just d
  _ -> Nothing

-- takeaway :: InvPrism { at ∷ String } Fulfillment
takeaway = invConstructor "Takeaway" Takeaway $ case _ of
  Takeaway t -> Just t
  _ -> Nothing

-- dineIn :: InvPrism Unit Fulfillment
dineIn = invConstructor "Dinein" (const DineIn) $ case _ of
  DineIn -> Just unit
  _ -> Nothing

-- coords :: InvPrism { lat ∷ String , long ∷ String } Place
coords = invConstructor "Coords" Coords $ case _ of
  Coords c -> Just c
  _ -> Nothing

-- address :: InvPrism { city ∷ String , street ∷ String , streetNumber ∷ String } Place
address = invConstructor "Address" Address $ case _ of
  Address a -> Just a
  _ -> Nothing

-- long :: forall a r . InvLens a { long ∷ a | r }
long = invField @"long"

-- lat :: forall a r . InvLens a { lat ∷ a | r }
lat = invField @"lat"

-- city :: forall a r . InvLens a { city ∷ a | r }
city = invField @"city"

-- street :: forall a r . InvLens a { street ∷ a | r }
street = invField @"street"

-- streetNumber :: forall a r . InvLens a { streetNumber ∷ a | r }
streetNumber = invField @"streetNumber"

-- at :: forall a r . InvLens a { at ∷ a | r }
at = invField @"at"

-- to :: forall a r . InvLens a { to ∷ a | r }
to = invField @"to"

-- product :: forall a r . InvLens a { product ∷ a | r }
product = invField @"product"

-- qty :: forall a r . InvLens a { qty ∷ a | r }
qty = invField @"qty"


-- fulfillment :: forall a r . InvLens a { fulfillment ∷ a | r }
fulfillment = invField @"fulfillment"

-- paymentMethod :: forall a r . InvLens a { paymentMethod ∷ a | r }
paymentMethod = invField @"paymentMethod"

-- hasNote :: forall r . InvLens Boolean { note ∷ Maybe String | r }
hasNote = invLens (\order -> isJust order.note) (\order -> case _ of
  true -> order { note = Just ""}
  false -> order { note = Nothing })

-- note :: forall a r . InvLens a { note ∷ a | r }
note = invField @"note"

-- paymentMethod' :: forall a r . InvLens a { paymentMethod :: a | r }
paymentMethod' = invField @"paymentMethod"

-- customer :: forall a r . InvLens a { customer ∷ a | r }
customer = invField @"customer"

-- card :: forall i r . InvCartesian i => InvCocartesian i => i Boolean → i { fulfillment ∷ Fulfillment , paymentMethod ∷ PaymentMethod | r }
card = invAffineTraversal (\order bool -> if bool then order { paymentMethod = Card } else order) (\order -> case order.fulfillment of
  Delivery _ -> Left $ order
  _ -> Right $ order.paymentMethod == Card
 )

-- cash :: forall i r . InvCartesian i => InvCocartesian i => i Boolean → i { fulfillment ∷ Fulfillment , paymentMethod ∷ PaymentMethod | r }
cash = invAffineTraversal (\order bool -> if bool then order { paymentMethod = Cash } else order) (\order -> case order.fulfillment of
  _ -> Right $ order.paymentMethod == Cash
 )

-- isDelivery :: InvLens Boolean Fulfillment
isDelivery = flip invLens (\ff bool -> if bool then Delivery { to: Address {city: "", street: "", streetNumber: ""}, at: "12:15"} else ff) (case _ of
  Delivery _ -> true
  _ -> false
 )

-- isDineIn :: InvLens Boolean Fulfillment
isDineIn = flip invLens (\ff bool -> if bool then DineIn else ff) (case _ of
  DineIn -> true
  _ -> false
 )

-- isTakeaway :: InvLens Boolean Fulfillment
isTakeaway = flip invLens (\ff bool -> if bool then Takeaway { at: "12:15" } else ff) (case _ of
  Takeaway _ -> true
  _ -> false
 )

