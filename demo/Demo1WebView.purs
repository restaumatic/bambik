module Demo1WebView
  ( order
  , customer
  ) where

import Prelude

import Demo1Business
import Web
import Web.MDC as MDC

order ∷ Widget Order Order
order =
  h1' (
    text # orderCaption ^
    chars " " ^
    text # shortId ) ^
  div' (
    MDC.filledTextField
      (text # uniqueIdCaption)) # uniqueId ^
  div' (
    MDC.filledTextField
      (text # shortIdCaption)) # shortId ^
  div' (
    h3' (chars "Order Id") ^
    MDC.filledTextField
      (text # shortCaption) # short ^
    MDC.filledTextField
      (text # uniqueCaption) # unique) # orderId ^
  div' (
    h3' (chars "Ordered by") ^
    customer # orderedBy) ^
  div' (
    MDC.checkbox # paid ^
    text # paidCaption) ^
  ( ( MDC.radioButton # isDineIn ^
      text # dineInCaption) ^
    ( MDC.radioButton # isTakeaway ^
      text # takeawayCaption) ^
    ( MDC.radioButton # isDelivery ^
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
    text # orderIdCaption ^
    chars " " ^
    text # short # orderId ^
    chars " (" ^
    text # unique # orderId ^
    chars ") " ^
    (
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

customer :: Widget CustomerInformal CustomerInformal
customer =
  div' (
    h4' (chars "Informal") ^
    MDC.filledTextField
      (text # firstNameCaption) # firstName ^
    MDC.filledTextField
      (text # lastNameCaption) # lastName) ^
  div' (
    h4' (chars "Formal") ^
    MDC.filledTextField
      (text # forenameCaption) # forename ^
    MDC.filledTextField
      (text # surnameCaption) # surname) # formal
