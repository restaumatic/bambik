module Demo1 where

import Prelude

import Data.Array (reverse)
import Data.Invariant (class Cartesian, class Invariant)
import Data.Invariant.Optics (invAdapter, invProjection)
import Data.Invariant.Transformers.Scoped (Scoped, invField')
import Data.Plus ((^))
import Data.String (toUpper)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Type.Proxy (Proxy(..))
import Web (WebComponentWrapper, div, runMainComponent)
import Web.HTML (staticText, text)
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

id :: forall i a b . Cartesian i => i (Scoped a) -> i (Scoped { id ∷ a | b })
id = invField' (Proxy :: Proxy "id")

customer :: forall i a b . Cartesian i => i (Scoped a) -> i (Scoped { customer ∷ a | b })
customer = invField' (Proxy :: Proxy "customer")

firstName :: forall i a b . Cartesian i => i (Scoped a) -> i (Scoped { firstName ∷ a | b })
firstName = invField' (Proxy :: Proxy "firstName")

lastName :: forall i a b . Cartesian i => i (Scoped a) -> i (Scoped { lastName ∷ a | b })
lastName = invField' (Proxy :: Proxy "lastName")

items :: forall i a b . Cartesian i => i (Scoped a) -> i (Scoped { items ∷ a | b })
items = invField' (Proxy :: Proxy "items")

name :: forall i a b . Cartesian i => i (Scoped a) -> i (Scoped { name ∷ a | b })
name = invField' (Proxy :: Proxy "name")

upperCase :: forall i . Cartesian i => i String -> i String
upperCase = invProjection toUpper

reversed ∷ forall i. Invariant i ⇒ i String → i String
reversed = invAdapter reverseString reverseString
  where
    reverseString ∷ String → String
    reverseString = toCharArray >>> reverse >>> fromCharArray

-- View (uses model)

orderComponent ∷ WebComponentWrapper Order
orderComponent =
  div $ MDC.filledText "Id" # id
  ^
  div (
    MDC.filledText "First name" # firstName
    ^
    MDC.filledText "Last name" # lastName
  ) # customer
  ^
  -- MDC.list itemComponent # (div # unsafeThrow "!") # items
  (
  div $ staticText "Summary: "
    ^ text # id
    ^ staticText " "
    ^ text # firstName # customer
    ^ staticText " "
    -- ^ text # upperCase # lastName # customer
    ^ text # lastName # customer
    ^ staticText ": "
  )
    -- ^ text # invlift # invProjection (intercalate ", ") #* name # items

itemComponent :: WebComponentWrapper Item
-- itemComponent = MDC.filledText "Name" # invlift # reversed # reversed # name
itemComponent = MDC.filledText "Name" # name

-- Glue (uses data and view)

main :: Effect Unit
main = do
  updateOrder <- runMainComponent orderComponent
  updateOrder { id: "61710", customer: { firstName: "John", lastName: "Doe"}, items: [ {name : "a"}, {name : "b"}, {name : "c"}]}
