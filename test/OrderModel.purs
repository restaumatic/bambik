module Test.OrderModel
  ( Addition
  , Fulfillment
  , Hour
  , Item
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
import Data.Generic.Rep (class Generic)
import Data.Lens (lens, only, prism')
import Data.Lens.AffineTraversal (affineTraversal)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy(..))

type Order =
  { id :: String
  , fulfillment :: Fulfillment
  , items :: Array Item
  , paymentMethod :: PaymentMethod
  , customer :: String
  , note :: Maybe String
  }

data PaymentMethod = Cash | Card

derive instance Generic PaymentMethod _
derive instance Eq PaymentMethod
instance Show PaymentMethod where
  show = genericShow


data Fulfillment
  = DineIn
  | Takeaway
    { at :: Hour}
  | Delivery
    { to :: Place
    , at :: Hour
    }
derive instance Generic Fulfillment _
derive instance Eq Fulfillment
instance Show Fulfillment where
  show = genericShow

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

derive instance Generic Place _
derive instance Eq Place

instance Show Place where
  show = genericShow

type Product = String

type Addition = String

-- optics
id = prop (Proxy :: Proxy "id")
items = prop (Proxy :: Proxy "items")
delivery = prism' Delivery $ case _ of
  Delivery d -> Just d
  _ -> Nothing
takeaway = prism' Takeaway $ case _ of
  Takeaway t -> Just t
  _ -> Nothing
dineIn = only DineIn
coords = prism' Coords $ case _ of
  Coords c -> Just c
  _ -> Nothing
address = prism' Address $ case _ of
  Address a -> Just a
  _ -> Nothing
long = prop (Proxy :: Proxy "long")
lat = prop (Proxy :: Proxy "lat")
city = prop (Proxy :: Proxy "city")
street = prop (Proxy :: Proxy "street")
streetNumber = prop (Proxy :: Proxy "streetNumber")
at = prop (Proxy :: Proxy "at")
to = prop (Proxy :: Proxy "to")
product = prop (Proxy :: Proxy "product")
qty = prop (Proxy :: Proxy "qty")
fulfillment = prop (Proxy :: Proxy "fulfillment")
paymentMethod = prop (Proxy :: Proxy "paymentMethod")
hasNote = lens (\order -> isJust order.note) (\order -> case _ of
  true -> order { note = Just ""}
  false -> order { note = Nothing })

note = prop (Proxy :: Proxy "note")
paymentMethod' = prop (Proxy :: Proxy "paymentMethod")
customer = prop (Proxy :: Proxy "customer")

card = affineTraversal (\order bool -> if bool then order { paymentMethod = Card } else order) (\order -> case order.fulfillment of
  Delivery _ -> Left $ order
  _ -> Right $ order.paymentMethod == Card
 )

cash = affineTraversal (\order bool -> if bool then order { paymentMethod = Cash } else order) (\order -> case order.fulfillment of
  _ -> Right $ order.paymentMethod == Cash
 )

isDelivery = flip lens (\ff bool -> if bool then Delivery { to: Address {city: "", street: "", streetNumber: ""}, at: "12:15"} else ff) (case _ of
  Delivery _ -> true
  _ -> false
 )

isDineIn = flip lens (\ff bool -> if bool then DineIn else ff) (case _ of
  DineIn -> true
  _ -> false
 )

isTakeaway = flip lens (\ff bool -> if bool then Takeaway { at: "12:15" } else ff) (case _ of
  Takeaway _ -> true
  _ -> false
 )

