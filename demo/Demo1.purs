module Demo1 (main) where

import Prelude hiding (div)

import Data.Invariant.Transformers.Scoped (adapter, constructor, field, projection)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Plus ((<^), (^))
import Demo1Business (CustomerInformal, Fulfillment(..), Order, formal)
import Effect (Effect)
import Web (Component, div', value, runMainComponent, text)
import Web.MDC as MDC

main :: Effect Unit
main = runMainComponent order
  { id: "61710"
  , customer:
    { firstName: "John"
    , lastName: "Doe"
    }
  , paid: true
  , fulfillment: DineIn
  }

order ∷ Component Order Order
order =
  div' (
    MDC.filledText "Id" # field @"id")
  ^
  div' (
    customer # field @"customer")
  ^
  div' (
    MDC.checkbox # field @"paid")
  ^
  div' (
    MDC.radioButton
    <^
    text "Dine in") # adapter "dine-in" (const DineIn) (case _ of
        DineIn -> true
        _ -> false) # field @"fulfillment"
  ^
  div' (
    MDC.radioButton
    <^
    text "Takeaway") # adapter "takeaway" (const Takeaway) (case _ of
        Takeaway -> true
        _ -> false) # field @"fulfillment"
  ^
  div' (
    MDC.radioButton
    <^
    text "Delivery"
    ) # adapter "delivery" (const (Delivery { address: "" })) (case _ of
        Delivery _ -> true
        _ -> false) # field @"fulfillment"
  ^
  div' (
    div' (
      text "Delivery details")
    ^
    div' (
      MDC.filledText "Address") # field @"address") # constructor "delivery" Delivery (case _ of
      Delivery c -> Just c
      _ -> Nothing) # field @"fulfillment"
  ^
  div' (
    text "Summary: "
    ^
    text # value # field @"id"
    ^
    text " "
    ^ (
      text # value # field @"firstName"
      ^ text " "
      ^ text # value # field @"lastName"
      ^ (
        text " ("
        ^ text # value # field @"forename"
        ^ text " "
        ^ text # value # field @"surname"
        ^ text ") ") # formal) # field @"customer"
    ^
    text ", paid: "
    ^
    text # value # projection "show" show # field @"paid"
    ^
    text ", fulfillment: "
    ^
    text # value # projection "show" show # field @"fulfillment"
  )

customer :: Component CustomerInformal CustomerInformal
customer =
  div' (
    MDC.filledText "First name" # field @"firstName"
    ^
    MDC.filledText "Last name" # field @"lastName")
  ^
  text "or more formally:"
  ^
  div' (
    MDC.filledText "Forename" # field @"forename"
    ^
    MDC.filledText "Surename" # field @"surname") # formal
