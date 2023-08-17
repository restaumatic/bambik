module Demo1 where

import Prelude hiding (div)

import Data.Array (length, reverse)
import Data.Invariant.Transformers.Scoped (Scoped, adapter, field, projection)
import Data.Plus ((<^), (^^))
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
  , fulfillment :: Fulfillment
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

data Fulfillment = DineIn | Takeaway | Delivery

instance Show Fulfillment where
  show DineIn = "Dine in"
  show Takeaway = "Takeaway"
  show Delivery = "Delivery"


formal :: forall i. Profunctor i => i (Scoped CustomerFormal) (Scoped CustomerFormal) -> i (Scoped CustomerInformal) (Scoped CustomerInformal)
formal = adapter "formal" toInformal toFormal
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
  div $ MDC.filledText "Id" # field @"id"
  ^^
  div $ customerComponent # field @"customer"
  ^^
  div $ MDC.checkbox # field @"paid"
  ^^
  div
    ( MDC.radioButton
    <^ text "Dine in"
    ) # adapter "dine-in" (const DineIn) (case _ of
        DineIn -> true
        _ -> false) # field @"fulfillment"
  ^^
  div
    ( MDC.radioButton
    <^ text "Takeaway"
    ) # adapter "takeaway" (const Takeaway) (case _ of
        Takeaway -> true
        _ -> false
        ) # field @"fulfillment"
  ^^
  div
    ( MDC.radioButton
    <^ text "Delivery"
    ) # adapter "delivery" (const Delivery) (case _ of
        Delivery -> true
        _ -> false) # field @"fulfillment"
  -- ^^
  -- MDC.list itemComponent # (div # unsafeThrow "!") # items
  ^^
  div $ text "Summary: "
    ^^ text # dynamic # field @"id"
    ^^ text " "
    ^^
    (
      text # dynamic # field @"firstName"
      ^^ text " "
      ^^ text # dynamic # field @"lastName"
      ^^
      (
        text " ("
        ^^ text # dynamic # field @"forename"
        ^^ text " "
        ^^ text # dynamic # field @"surname"
        ^^ text ") "
      ) # formal
    ) # field @"customer"
    ^^ text ", paid: "
    ^^ text # dynamic # projection "show" show # field @"paid"
    ^^ text ", fulfillment: "
    ^^ text # dynamic # projection "show" show # field @"fulfillment"
    ^^ text ", no of items: "
    ^^ text # dynamic # projection "show" show # projection "length" length # field @"items"

customerComponent :: WebComponentWrapper CustomerInformal CustomerInformal
customerComponent =
  (
    div
      (
      MDC.filledText "First name" # field @"firstName"
      ^^
      MDC.filledText "Last name" # field @"lastName"
      )
    ^^
    text "or more formally:"
    ^^
    div
      (
      MDC.filledText "Forename" # field @"forename"
      ^^
      MDC.filledText "Surename" # field @"surname"
      ) # formal
  )

itemComponent :: WebComponentWrapper Item Item
itemComponent = MDC.filledText "Name" # adapter "reverse" reverseString reverseString # adapter "reverse" reverseString reverseString # field @"name"

-- Glue (business + view)

main :: Effect Unit
main = do
  updateOrder <- runMainComponent orderComponent
  updateOrder { id: "61710", customer: { firstName: "John", lastName: "Doe"}, items: [ {name : "a"}, {name : "b"}, {name : "c"}], paid: true, fulfillment: Delivery}
