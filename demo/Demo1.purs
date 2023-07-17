module Demo1 where

import Prelude

import Data.Array (reverse)
import Data.Foldable (intercalate)
import Data.Invariant (class Cartesian, class Invariant)
import Data.Invariant.Optics (invAdapter, invProjection)
import Data.Invariant.Optics.Tagged (class Tagged, invField)
import Data.Invariant.Transformers (invlift, (#*))
import Data.Plus ((^))
import Data.String (toUpper)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Type.Proxy (Proxy(..))
import Web (Component, inside, runMainComponent)
import Web.HTML as HTML
import Web.MDC as MDC

type Order =
      { id :: String
      , customer :: Customer
      , items :: Array String
      }

type Customer =
      { firstName :: String
      , lastName :: String
      }

id :: forall i a b . Invariant i => Cartesian i => Tagged i => i a -> i { id ∷ a | b }
id = invField (Proxy :: Proxy "id")

customer :: forall i a b . Invariant i => Cartesian i => Tagged i => i a -> i { customer ∷ a | b }
customer = invField (Proxy :: Proxy "customer")

firstName :: forall i a b . Invariant i => Cartesian i => Tagged i => i a -> i { firstName ∷ a | b }
firstName = invField (Proxy :: Proxy "firstName")

lastName :: forall i a b . Invariant i => Cartesian i => Tagged i => i a -> i { lastName ∷ a | b }
lastName = invField (Proxy :: Proxy "lastName")

items :: forall i a b . Invariant i => Cartesian i => Tagged i => i a -> i { items ∷ a | b }
items = invField (Proxy :: Proxy "items")

upperCase :: forall i . Invariant i => Cartesian i => i String -> i String
upperCase = invProjection toUpper

reverseString ∷ String → String
reverseString = toCharArray >>> reverse >>> fromCharArray

--

orderComponent ∷ Component Order
orderComponent =
  customerComponent # inside "div" # customer
  ^
  MDC.list itemComponent # inside "div" #* invAdapter reverseString reverseString #* invAdapter reverseString reverseString # items
  ^
  HTML.staticText "Summary: "
    ^ HTML.text # id
    ^ HTML.staticText " "
    ^ HTML.text # firstName # customer
    ^ HTML.staticText " "
    ^ HTML.text # upperCase # lastName # customer
    ^ HTML.staticText ": "
    ^ HTML.text # invProjection (intercalate ", ") # items

customerComponent :: Component Customer
customerComponent =
  MDC.filledText "First name" # inside "div" # firstName
  ^
  MDC.filledText "Last name" # inside "div" # lastName

itemComponent :: Component String
itemComponent = MDC.filledText "Item"

main :: Effect Unit
main = do
  updateOrder <- runMainComponent orderComponent
  updateOrder { id: "61710", customer: { firstName: "John", lastName: "Doe"}, items: ["a", "b", "c"]}
