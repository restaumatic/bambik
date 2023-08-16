module Demo1 where

import Prelude hiding (div)

import Data.Array (length, reverse)
import Data.Invariant (class Invariant)
import Data.Invariant.Transformers.Scoped (Scoped, invAdapter, invField, invProjection)
import Data.Plus ((^))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Web (WebComponentWrapper, div, dynamic, runMainComponent, text)
import Web.MDC as MDC

-- business

type Order =
  { id :: String
  , customer :: CustomerInformal
  , items :: Array Item
  , paid :: Boolean
  }

type CustomerInformal =
  { firstName :: String
  , lastName :: String
  }

type Item =
  { name :: String
  }

type CustomerFormal =
  { forename :: String
  , surname :: String
  }

formal :: forall i. Invariant i => i (Scoped CustomerFormal) -> i (Scoped CustomerInformal)
formal = invAdapter "formal" toInformal toFormal
  where
    toFormal :: CustomerInformal -> CustomerFormal
    toFormal { firstName: forename, lastName: surname } = { forename, surname }
    toInformal :: CustomerFormal -> CustomerInformal
    toInformal { forename: firstName, surname: lastName } = { firstName, lastName }

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
    ^
    (
      text # dynamic # invField @"firstName"
      ^ text " "
      ^ text # dynamic # invField @"lastName"
      ^
      (
        text " ("
        ^ text # dynamic # invField @"forename"
        ^ text " "
        ^ text # dynamic # invField @"surname"
        ^ text ") "
      ) # formal
    ) # invField @"customer"
    ^ text ", paid: "
    ^ text # dynamic # invProjection "show" show # invField @"paid"
    ^ text ", no of items: "
    ^ text # dynamic # invProjection "show" show # invProjection "length" length # invField @"items"

customerComponent :: WebComponentWrapper CustomerInformal
customerComponent =
  (
    div
      (
      MDC.filledTextField "First name" # invField @"firstName"
      ^
      MDC.filledTextField "Last name" # invField @"lastName"
      )
    ^
    text "or more formally"
    ^
    div
      (
      MDC.filledTextField "Forename" # invField @"forename"
      ^
      MDC.filledTextField "Surename" # invField @"surname"
      ) # formal
  )

itemComponent :: WebComponentWrapper Item
itemComponent = MDC.filledTextField "Name" # invAdapter "reverse" reverseString reverseString # invAdapter "reverse" reverseString reverseString # invField @"name"

-- Glue (business + view)

main :: Effect Unit
main = do
  updateOrder <- runMainComponent orderComponent
  updateOrder { id: "61710", customer: { firstName: "John", lastName: "Doe"}, items: [ {name : "a"}, {name : "b"}, {name : "c"}], paid: true}
