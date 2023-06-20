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
  , numberOfItems
  , paymentMethod
  , paymentMethod'
  , placeOrder
  , product
  , qty
  , street
  , streetNumber
  , takeaway
  , to
  )
  where

import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Invariant (inveff)
import Data.Invariant.Optics (invAffineTraversal, invLens, invProjection)
import Data.Invariant.Optics.Tagged (invField, invConstructor)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (Aff, launchAff_)
import Type.Proxy (Proxy(..))

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
id = invField (Proxy :: Proxy "id")

-- items :: forall a r . InvLens a { items ∷ a | r }
items = invField (Proxy :: Proxy "items")

-- delivery :: InvPrism { at ∷ String , to ∷ Place } Fulfillment
delivery = invConstructor Delivery $ case _ of
  Delivery d -> Just d
  _ -> Nothing

-- takeaway :: InvPrism { at ∷ String } Fulfillment
takeaway = invConstructor Takeaway $ case _ of
  Takeaway t -> Just t
  _ -> Nothing

-- dineIn :: InvPrism Unit Fulfillment
dineIn = invConstructor (const DineIn) $ case _ of
  DineIn -> Just unit
  _ -> Nothing

-- coords :: InvPrism { lat ∷ String , long ∷ String } Place
coords = invConstructor Coords $ case _ of
  Coords c -> Just c
  _ -> Nothing

-- address :: InvPrism { city ∷ String , street ∷ String , streetNumber ∷ String } Place
address = invConstructor Address $ case _ of
  Address a -> Just a
  _ -> Nothing

-- long :: forall a r . InvLens a { long ∷ a | r }
long = invField (Proxy :: Proxy "long")

-- lat :: forall a r . InvLens a { lat ∷ a | r }
lat = invField (Proxy :: Proxy "lat")

-- city :: forall a r . InvLens a { city ∷ a | r }
city = invField (Proxy :: Proxy "city")

-- street :: forall a r . InvLens a { street ∷ a | r }
street = invField (Proxy :: Proxy "street")

-- streetNumber :: forall a r . InvLens a { streetNumber ∷ a | r }
streetNumber = invField (Proxy :: Proxy "streetNumber")

-- at :: forall a r . InvLens a { at ∷ a | r }
at = invField (Proxy :: Proxy "at")

-- to :: forall a r . InvLens a { to ∷ a | r }
to = invField (Proxy :: Proxy "to")

-- product :: forall a r . InvLens a { product ∷ a | r }
product = invField (Proxy :: Proxy "product")

-- qty :: forall a r . InvLens a { qty ∷ a | r }
qty = invField (Proxy :: Proxy "qty")


-- fulfillment :: forall a r . InvLens a { fulfillment ∷ a | r }
fulfillment = invField (Proxy :: Proxy "fulfillment")

-- paymentMethod :: forall a r . InvLens a { paymentMethod ∷ a | r }
paymentMethod = invField (Proxy :: Proxy "paymentMethod")

-- hasNote :: forall r . InvLens Boolean { note ∷ Maybe String | r }
hasNote = invLens (\order -> isJust order.note) (\order -> case _ of
  true -> order { note = Just ""}
  false -> order { note = Nothing })

-- note :: forall a r . InvLens a { note ∷ a | r }
note = invField (Proxy :: Proxy "note")

-- paymentMethod' :: forall a r . InvLens a { paymentMethod :: a | r }
paymentMethod' = invField (Proxy :: Proxy "paymentMethod")

-- customer :: forall a r . InvLens a { customer ∷ a | r }
customer = invField (Proxy :: Proxy "customer")

-- card :: forall i r . Cartesian i => CoCartesian i => i Boolean → i { fulfillment ∷ Fulfillment , paymentMethod ∷ PaymentMethod | r }
card = invAffineTraversal (\order bool -> if bool then order { paymentMethod = Card } else order) (\order -> case order.fulfillment of
  Delivery _ -> Left $ order
  _ -> Right $ order.paymentMethod == Card
 )

-- cash :: forall i r . Cartesian i => CoCartesian i => i Boolean → i { fulfillment ∷ Fulfillment , paymentMethod ∷ PaymentMethod | r }
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

-- placeOrder :: forall i . EffInvariant i => i Order -> i Order
placeOrder = inveff (\order -> launchAff_ $ doPlaceOrder order)
  where
    doPlaceOrder :: Order -> Aff Unit
    doPlaceOrder = mempty

-- numberOfItems :: forall i . Cartesian i => i Int -> i Order
numberOfItems = invProjection (\order -> length order.items)
