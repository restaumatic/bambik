module Demo1 where

import Prelude hiding (div)

import Data.Array (length)
import Data.Invariant.Transformers.Scoped (Scoped, adapterGeneric, constructor, field, iso, projection)
import Data.Maybe (Maybe(..))
import Data.Plus ((<^), (^^))
import Data.Profunctor (class Profunctor)
import Effect (Effect)
import Web (WebComponentWrapper, div, dynamic, nothing, runMainComponent, text)
import Web.MDC as MDC

-- Business

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

data Fulfillment = DineIn | Takeaway | Delivery { address :: Address }

type Address = String

instance Show Fulfillment where
  show DineIn = "Dine in"
  show Takeaway = "Takeaway"
  show (Delivery { address }) = "Delivery to " <> address


formal :: forall i. Profunctor i => i (Scoped CustomerFormal) (Scoped CustomerFormal) -> i (Scoped CustomerInformal) (Scoped CustomerInformal)
formal = iso "formal" toInformal toFormal
  where
    toFormal :: CustomerInformal -> CustomerFormal
    toFormal { firstName: forename, lastName: surname } = { forename, surname }
    toInformal :: CustomerFormal -> CustomerInformal
    toInformal { forename: firstName, surname: lastName } = { firstName, lastName }

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
    ) # adapterGeneric "dine-in" (const DineIn) (case _ of
        DineIn -> true
        _ -> false) # field @"fulfillment"
  ^^
  div
    ( MDC.radioButton
    <^ text "Takeaway"
    ) # adapterGeneric "takeaway" (const Takeaway) (case _ of
        Takeaway -> true
        _ -> false
        ) # field @"fulfillment"
  ^^
  div
    ( MDC.radioButton
    <^ text "Delivery"
    ) # adapterGeneric "delivery" (const (Delivery { address: "" })) (case _ of
        Delivery _ -> true
        _ -> false) # field @"fulfillment"
  ^^
  div $ MDC.filledText "Foo" # nothing
  ^^
  div $ MDC.filledText "Address" # field @"address" # constructor "delivery" Delivery (case _ of
        Delivery c -> Just c
        _ -> Nothing) # field @"fulfillment"
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
itemComponent = MDC.filledText "Name" # field @"name"

-- Glue (business + view)

main :: Effect Unit
main = do
  updateOrder <- runMainComponent orderComponent
  updateOrder { id: "61710", customer: { firstName: "John", lastName: "Doe"}, items: [ {name : "a"}, {name : "b"}, {name : "c"}], paid: true, fulfillment: Delivery { address: ""}}
