module Demo1 where

import Prelude

import Data.Array (reverse)
import Data.Invariant (class Cartesian, class Filtered, class Invariant)
import Data.Invariant.Optics (invAdapter, invProjection)
import Data.Invariant.Transformers (invlift, invliftmap)
import Data.Invariant.Transformers.Scoped (Scoped, invField')
import Data.Newtype (modify)
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
  MDC.filledText "Id" # invlift # invliftmap (inside "div") # id
  ^
  (
    MDC.filledText "First name" # invlift # invliftmap (inside "div") # firstName
    ^
    MDC.filledText "Last name" # invlift # invliftmap (inside "div") # lastName
  ) # invliftmap (inside "div") # customer
  ^
  -- MDC.list itemComponent # (inside "div" # unsafeThrow "!") # items
  (
  HTML.staticText "Summary: " # invlift
    ^ HTML.text # invlift # id
    ^ HTML.staticText " " # invlift
    ^ HTML.text # invlift # firstName # customer
    ^ HTML.staticText " " # invlift
    ^ HTML.text # invlift # upperCase # lastName # customer
    ^ HTML.staticText ": " # invlift
  ) # modify (inside "div")
    -- ^ HTML.text # invlift # invProjection (intercalate ", ") #* name # items

itemComponent :: WebUI Item
itemComponent = MDC.filledText "Name" # invlift # reversed # reversed # name

-- Glue (uses data and view)

main :: Effect Unit
main = do
  updateOrder <- runMainComponent orderComponent
  updateOrder { id: "61710", customer: { firstName: "John", lastName: "Doe"}, items: [ {name : "a"}, {name : "b"}, {name : "c"}]}
