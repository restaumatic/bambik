module Demo1 where

import Prelude

import Data.Array (reverse)
import Data.Foldable (intercalate)
import Data.Invariant (class Cartesian, class Filtered, class Invariant)
import Data.Invariant.Optics (invAdapter, invProjection)
import Data.Invariant.Transformers (Scoped, invField', scoped, (#*))
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

id :: forall i a b . Filtered i => Cartesian i => Scoped i a -> Scoped i { id ∷ a | b }
id = invField' (Proxy :: Proxy "id")

customer :: forall i a b . Filtered i => Cartesian i => Scoped i a -> Scoped i { customer ∷ a | b }
customer = invField' (Proxy :: Proxy "customer")

firstName :: forall i a b . Filtered i => Cartesian i => Scoped i a -> Scoped i { firstName ∷ a | b }
firstName = invField' (Proxy :: Proxy "firstName")

lastName :: forall i a b . Filtered i => Cartesian i => Scoped i a -> Scoped i { lastName ∷ a | b }
lastName = invField' (Proxy :: Proxy "lastName")

items :: forall i a b . Filtered i => Cartesian i => Scoped i a -> Scoped i { items ∷ a | b }
items = invField' (Proxy :: Proxy "items")

name :: forall i a b . Filtered i => Cartesian i => Scoped i a -> Scoped i { name ∷ a | b }
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
  customerComponent #* inside "div" # customer
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
  MDC.filledText "First name" # inside "div" # scoped # firstName
  ^
  MDC.filledText "Last name" # inside "div" # scoped # lastName

itemComponent :: WebUI Item
itemComponent = MDC.filledText "Name" # scoped # reversed # reversed # name

-- Glue (uses data and view)

main :: Effect Unit
main = do
  updateOrder <- runMainComponent orderComponent
  updateOrder { id: "61710", customer: { firstName: "John", lastName: "Doe"}, items: [ {name : "a"}, {name : "b"}, {name : "c"}]}
