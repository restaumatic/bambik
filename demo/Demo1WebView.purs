module Demo1WebView
  ( order
  , customer
  ) where

import Prelude

import Demo1Business
import Web
import Web.MDC as MDC

order ∷ Component Order
order =
  div' (
    customer # orderedBy) ^
  div' (
    MDC.checkbox # paid ^
    text "Paid") ^
  (div' (
      MDC.radioButton # isDineIn ^
      text "Dine in") ^
    div' (
      MDC.radioButton # isTakeaway ^
      text "Takeaway") ^
    div' (
      MDC.radioButton # isDelivery ^
      text "Delivery")) # fulfillment ^
  div' (
    div' (
      text "Delivery details") ^
    div' (
      MDC.filledTextField "Address") # address) # delivery # fulfillment ^
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
    text # value # print # paid ^
    text ", fulfillment: " ^
    text # value # print # fulfillment) ^
  div' (
    MDC.containedButton submit (
      text "Submit order"
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
