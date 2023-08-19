module Demo1WebView
  ( order
  , customer
  ) where

import Demo1Business
import Prelude
import Web

import Web.MDC as MDC

order ∷ Component Order
order =
  div' (
    MDC.filledTextField "ID" # id) ^
  div' (
    customer # orderedBy) ^
  div' (
    MDC.checkbox # paid ^
    text "Paid") ^
  ( div' (
      MDC.radioButton # isDineIn ^
      text "Dine in") ^
    div' (
      MDC.radioButton # isTakeaway ^
      text "Takeaway") ^
    div' (
      MDC.radioButton # isDelivery ^
      text "Delivery")) # fulfillment ^
  ( div'
      ( MDC.filledTextField "Table" # table) # dineIn ^
    div'
      ( MDC.filledTextField "Time" # time) # takeaway ^
    div'
      ( MDC.filledTextField "Address" # address) # delivery) # fulfillment ^
  div' (
    text "Summary: " ^
    text # value # id ^
    text " " ^ (
      text # value # firstName ^
      text " " ^
      text # value # lastName ^ (
        text " (" ^
        text # value # forename ^
        text " " ^
        text # value # surname ^
        text ") ") # formal) # orderedBy ^
    text ", paid: " ^
    text # value # paymentStatus ^
    text ", fulfillment: " ^
    text # value # fulfillmentData) ^
  div' (
    MDC.containedButton writeOrderToConsole (
      text "Write order to console"
    )
  )

customer :: Component CustomerInformal
customer =
  div' (
    MDC.filledTextField "First name" # firstName ^
    MDC.filledTextField "Last name" # lastName) ^
  text "or more formally:" ^
  div' (
    MDC.filledTextField "Forename" # forename ^
    MDC.filledTextField "Surename" # surname) # formal
