module Demo1 where

import Prelude hiding (div)

import Data.Array (length, reverse)
import Data.Invariant.Transformers.Scoped (Scoped, proAdapter, proField, proProjection)
import Data.Plus ((^^))
import Data.Profunctor (class Profunctor)
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

formal :: forall i. Profunctor i => i (Scoped CustomerFormal) (Scoped CustomerFormal) -> i (Scoped CustomerInformal) (Scoped CustomerInformal)
formal = proAdapter "formal" toInformal toFormal
  where
    toFormal :: CustomerInformal -> CustomerFormal
    toFormal { firstName: forename, lastName: surname } = { forename, surname }
    toInformal :: CustomerFormal -> CustomerInformal
    toInformal { forename: firstName, surname: lastName } = { firstName, lastName }

reverseString ∷ String -> String
reverseString = toCharArray >>> reverse >>> fromCharArray

-- View (uses business)

orderComponent ∷ WebComponentWrapper Order Order
orderComponent =
  div $ MDC.filledTextField "Id" # proField @"id"
  ^^
  div $ customerComponent # proField @"customer"
  ^^
  div $ MDC.checkbox # proField @"paid"
  -- ^^
  -- MDC.list itemComponent # (div # unsafeThrow "!") # items
  ^^
  div $ text "Summary: "
    ^^ text # dynamic # proField @"id"
    ^^ text " "
    ^^
    (
      text # dynamic # proField @"firstName"
      ^^ text " "
      ^^ text # dynamic # proField @"lastName"
      ^^
      (
        text " ("
        ^^ text # dynamic # proField @"forename"
        ^^ text " "
        ^^ text # dynamic # proField @"surname"
        ^^ text ") "
      ) # formal
    ) # proField @"customer"
    ^^ text ", paid: "
    ^^ text # dynamic # proProjection "show" show # proField @"paid"
    ^^ text ", no of items: "
    ^^ text # dynamic # proProjection "show" show # proProjection "length" length # proField @"items"

customerComponent :: WebComponentWrapper CustomerInformal CustomerInformal
customerComponent =
  (
    div
      (
      MDC.filledTextField "First name" # proField @"firstName"
      ^^
      MDC.filledTextField "Last name" # proField @"lastName"
      )
    ^^
    text "or more formally"
    ^^
    div
      (
      MDC.filledTextField "Forename" # proField @"forename"
      ^^
      MDC.filledTextField "Surename" # proField @"surname"
      ) # formal
  )

itemComponent :: WebComponentWrapper Item Item
itemComponent = MDC.filledTextField "Name" # proAdapter "reverse" reverseString reverseString # proAdapter "reverse" reverseString reverseString # proField @"name"

-- Glue (business + view)

main :: Effect Unit
main = do
  updateOrder <- runMainComponent orderComponent
  updateOrder { id: "61710", customer: { firstName: "John", lastName: "Doe"}, items: [ {name : "a"}, {name : "b"}, {name : "c"}], paid: true}
