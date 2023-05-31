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
import Data.Invariant.Optics (constructorInvPrism, invAffineTraversal, invLens, projection, propertyInvLens)
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
id = propertyInvLens (Proxy :: Proxy "id")

-- items :: forall a r . InvLens a { items ∷ a | r }
items = propertyInvLens (Proxy :: Proxy "items")

-- delivery :: InvPrism { at ∷ String , to ∷ Place } Fulfillment
delivery = constructorInvPrism Delivery $ case _ of
  Delivery d -> Just d
  _ -> Nothing

-- takeaway :: InvPrism { at ∷ String } Fulfillment
takeaway = constructorInvPrism Takeaway $ case _ of
  Takeaway t -> Just t
  _ -> Nothing

-- dineIn :: InvPrism Unit Fulfillment
dineIn = constructorInvPrism (const DineIn) $ case _ of
  DineIn -> Just unit
  _ -> Nothing

-- coords :: InvPrism { lat ∷ String , long ∷ String } Place
coords = constructorInvPrism Coords $ case _ of
  Coords c -> Just c
  _ -> Nothing

-- address :: InvPrism { city ∷ String , street ∷ String , streetNumber ∷ String } Place
address = constructorInvPrism Address $ case _ of
  Address a -> Just a
  _ -> Nothing

-- long :: forall a r . InvLens a { long ∷ a | r }
long = propertyInvLens (Proxy :: Proxy "long")

-- lat :: forall a r . InvLens a { lat ∷ a | r }
lat = propertyInvLens (Proxy :: Proxy "lat")

-- city :: forall a r . InvLens a { city ∷ a | r }
city = propertyInvLens (Proxy :: Proxy "city")

-- street :: forall a r . InvLens a { street ∷ a | r }
street = propertyInvLens (Proxy :: Proxy "street")

-- streetNumber :: forall a r . InvLens a { streetNumber ∷ a | r }
streetNumber = propertyInvLens (Proxy :: Proxy "streetNumber")

-- at :: forall a r . InvLens a { at ∷ a | r }
at = propertyInvLens (Proxy :: Proxy "at")

-- to :: forall a r . InvLens a { to ∷ a | r }
to = propertyInvLens (Proxy :: Proxy "to")

-- product :: forall a r . InvLens a { product ∷ a | r }
product = propertyInvLens (Proxy :: Proxy "product")

-- qty :: forall a r . InvLens a { qty ∷ a | r }
qty = propertyInvLens (Proxy :: Proxy "qty")


-- fulfillment :: forall a r . InvLens a { fulfillment ∷ a | r }
fulfillment = propertyInvLens (Proxy :: Proxy "fulfillment")

-- paymentMethod :: forall a r . InvLens a { paymentMethod ∷ a | r }
paymentMethod = propertyInvLens (Proxy :: Proxy "paymentMethod")

-- hasNote :: forall r . InvLens Boolean { note ∷ Maybe String | r }
hasNote = invLens (\order -> isJust order.note) (\order -> case _ of
  true -> order { note = Just ""}
  false -> order { note = Nothing })

-- note :: forall a r . InvLens a { note ∷ a | r }
note = propertyInvLens (Proxy :: Proxy "note")

-- paymentMethod' :: forall a r . InvLens a { paymentMethod :: a | r }
paymentMethod' = propertyInvLens (Proxy :: Proxy "paymentMethod")

-- customer :: forall a r . InvLens a { customer ∷ a | r }
customer = propertyInvLens (Proxy :: Proxy "customer")

-- card :: forall i r . CartesianInvariant i => CoCartesianInvariant i => i Boolean → i { fulfillment ∷ Fulfillment , paymentMethod ∷ PaymentMethod | r }
card = invAffineTraversal (\order bool -> if bool then order { paymentMethod = Card } else order) (\order -> case order.fulfillment of
  Delivery _ -> Left $ order
  _ -> Right $ order.paymentMethod == Card
 )

-- cash :: forall i r . CartesianInvariant i => CoCartesianInvariant i => i Boolean → i { fulfillment ∷ Fulfillment , paymentMethod ∷ PaymentMethod | r }
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

-- numberOfItems :: forall i . CartesianInvariant i => i Int -> i Order
numberOfItems = projection (\order -> length order.items)
