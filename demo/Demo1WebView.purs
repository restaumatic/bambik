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
    MDC.filledTextField
      (text # idCaption) # id) ^
  div' (
    customer # orderedBy) ^
  div' (
    MDC.checkbox # paid ^
    text # paidCaption) ^
  ( div' (
      MDC.radioButton # isDineIn ^
      text # dineInCaption) ^
    div' (
      MDC.radioButton # isTakeaway ^
      text # takeawayCaption) ^
    div' (
      MDC.radioButton # isDelivery ^
      text # deliveryCaption)) # fulfillment ^
  ( div'
      ( MDC.filledTextField
        (text # tableCaption) # table) # dineIn ^
    div'
      ( MDC.filledTextField
        (text # timeCaption) # time) # takeaway ^
    div'
      ( MDC.filledTextField
        (text # addressCaption) # address) # delivery) # fulfillment ^
  div' (
    text # orderCaption ^
    chars ": " ^
    text # id ^
    chars " " ^ (
      text # firstName ^
      chars " " ^
      text # lastName ^ (
        chars " (" ^
        text # forename ^
        chars " " ^
        text # surname ^
        chars ") ") # formal) # orderedBy ^
    chars ", " ^
    text # paidCaption ^
    chars ": " ^
    text # paymentStatus # paid ^
    chars ", " ^
    text # fullfilmentCaption ^
    chars ": " ^
    text # fulfillmentData # fulfillment) ^
  div' (
    MDC.containedButton (
      text # writeOrderToConsoleCaption) writeOrderToConsole
  )

customer :: Component CustomerInformal
customer =
  div' (
    MDC.filledTextField
      (text # firstNameCaption) # firstName ^
    MDC.filledTextField
      (text # lastNameCaption) # lastName) ^
  div' (
    MDC.filledTextField
      (text # forenameCaption) # forename ^
    MDC.filledTextField
      (text # surnameCaption) # surname) # formal
