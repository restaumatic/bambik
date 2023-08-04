module Demo1 where

import Prelude

import Data.Array (reverse)
import Data.Foldable (intercalate)
import Data.Invariant (class Cartesian, class Invariant)
import Data.Invariant.Optics (invAdapter, invProjection)
import Data.Invariant.Transformers (invField', (#*))
import Data.Plus ((^))
import Data.String (toUpper)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Type.Proxy (Proxy(..))
import Web (WebUI, inside, runMainComponent)
import Web.HTML as HTML
import Web.MDC as MDC

-- data

type Order =
      { id :: String
      , customer :: Customer
      , items :: Array Item
      }

type Customer =
      { firstName :: String
      , lastName :: String
      }

type Item =
  { name :: String
  }

-- Model (uses data)

id :: forall i a b . Cartesian i => i a -> i { id ∷ a | b }
id = invField' (Proxy :: Proxy "id")

customer :: forall i a b . Cartesian i => i a -> i { customer ∷ a | b }
customer = invField' (Proxy :: Proxy "customer")

firstName :: forall i a b . Cartesian i => i a -> i { firstName ∷ a | b }
firstName = invField' (Proxy :: Proxy "firstName")

lastName :: forall i a b . Cartesian i => i a -> i { lastName ∷ a | b }
lastName = invField' (Proxy :: Proxy "lastName")

items :: forall i a b . Cartesian i => i a -> i { items ∷ a | b }
items = invField' (Proxy :: Proxy "items")

name :: forall i a b . Cartesian i => i a -> i { name ∷ a | b }
name = invField' (Proxy :: Proxy "name")

upperCase :: forall i . Cartesian i => i String -> i String
upperCase = invProjection toUpper

reversed ∷ forall i. Invariant i ⇒ i String → i String
reversed = invAdapter reverseString reverseString
  where
    reverseString ∷ String → String
    reverseString = toCharArray >>> reverse >>> fromCharArray

-- View (uses model)

orderComponent ∷ WebUI Order
orderComponent =
  customerComponent # inside "div" # customer
  ^
  MDC.list itemComponent # inside "div" # items
  ^
  HTML.staticText "Summary: "
    ^ HTML.text # id
    ^ HTML.staticText " "
    ^ HTML.text # firstName # customer
    ^ HTML.staticText " "
    ^ HTML.text # upperCase # lastName # customer
    ^ HTML.staticText ": "
    ^ HTML.text # invProjection (intercalate ", ") #* name # items

customerComponent :: WebUI Customer
customerComponent =
  MDC.filledText "First name" # inside "div" # firstName
  ^
  MDC.filledText "Last name" # inside "div" # lastName

itemComponent :: WebUI Item
itemComponent = MDC.filledText "Name" # reversed # reversed # name

-- Glue (uses data and view)

main :: Effect Unit
main = do
  updateOrder <- runMainComponent orderComponent
  updateOrder { id: "61710", customer: { firstName: "John", lastName: "Doe"}, items: [ {name : "a"}, {name : "b"}, {name : "c"}]}
