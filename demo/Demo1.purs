module Demo1 where

import Prelude hiding (div)

import Data.Array (length, reverse)
import Data.Invariant.Transformers.Scoped (invAdapter, invField, invProjection)
import Data.Plus ((^))
import Data.String (toUpper)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Web (WebComponentWrapper, div, dynamic, runMainComponent, text)
import Web.MDC as MDC

-- business

type Order =
  { id :: String
  , customer :: Customer
  , items :: Array Item
  , paid :: Boolean
  }

type Customer =
  { firstName :: String
  , lastName :: String
  }

type Item =
  { name :: String
  }

reverseString ∷ String -> String
reverseString = toCharArray >>> reverse >>> fromCharArray

-- View (uses business)

orderComponent ∷ WebComponentWrapper Order
orderComponent =
  div $ MDC.filledTextField "Id" # invField @"id"
  ^
  div $ customerComponent # invField @"customer"
  ^
  div $ MDC.checkbox # invField @"paid"
  -- ^
  -- MDC.list itemComponent # (div # unsafeThrow "!") # items
  ^
  div $ text "Summary: "
    ^ text # dynamic # invField @"id"
    ^ text " "
    ^ text # dynamic # invField @"firstName" # invField @"customer"
    ^ text " "
    ^ text # dynamic # invProjection "toUpper" toUpper # invField @"lastName" # invField @"customer"
    ^ text ", paid: "
    ^ text # dynamic # invProjection "show" show # invField @"paid"
    ^ text ", no of items: "
    ^ text # dynamic # invProjection "show" show # invProjection "length" length # invField @"items"

customerComponent :: WebComponentWrapper Customer
customerComponent =
  (
    MDC.filledTextField "First name" # invField @"firstName"
    ^
    MDC.filledTextField "Last name" # invField @"lastName"
  )

itemComponent :: WebComponentWrapper Item
itemComponent = MDC.filledTextField "Name" # invAdapter "reverse" reverseString reverseString # invAdapter "reverse" reverseString reverseString # invField @"name"

-- Glue (business + view)

main :: Effect Unit
main = do
  updateOrder <- runMainComponent orderComponent
  updateOrder { id: "61710", customer: { firstName: "John", lastName: "Doe"}, items: [ {name : "a"}, {name : "b"}, {name : "c"}], paid: true}
