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
    MDC.filledTextField uniqueIdCaption) # uniqueId ^
  div' (
    MDC.filledTextField shortIdCaption) # shortId ^
  div' (
    h3' (chars "Order Id") ^
    MDC.filledTextField shortCaption # short ^
    MDC.filledTextField uniqueCaption # unique) # orderId ^
  div' (
    h3' (chars "Ordered by") ^
    customer # orderedBy) ^
  div' (
    MDC.checkbox paidCaption # paid) ^
  ( MDC.radioButton dineInCaption # isDineIn ^
    MDC.radioButton takeawayCaption # isTakeaway ^
    MDC.radioButton deliveryCaption # isDelivery) # fulfillment ^
  ( div'
      ( MDC.filledTextField tableCaption # table) # dineIn ^
    div'
      ( MDC.filledTextField timeCaption # time) # takeaway ^
    div'
      ( MDC.filledTextField addressCaption # address) # delivery) # fulfillment ^
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
    MDC.containedButton (writeOrderToConsoleCaption >>> shortId) writeOrderToConsole
  )

customer :: Widget CustomerInformal CustomerInformal
customer =
  div' (
    h4' (chars "Informal") ^
    MDC.filledTextField firstNameCaption # firstName ^
    MDC.filledTextField lastNameCaption # lastName) ^
  div' (
    h4' (chars "Formal") ^
    MDC.filledTextField forenameCaption # forename ^
    MDC.filledTextField surnameCaption # surname) # formal
