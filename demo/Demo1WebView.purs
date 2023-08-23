module Demo1WebView
  ( order
  ) where

import Prelude

import Demo1Business
import Web
import Web.MDC as MDC

order ∷ Widget Order Order
order =
  MDC.elevation9
  ( MDC.headline4
      ( text # orderTitle # shortId) ^
    div'
      ( MDC.filledTextField uniqueIdCaption) # uniqueId ^
    div'
      ( MDC.filledTextField shortIdCaption) # shortId ^
    div'
      ( MDC.subtitle1
        ( text # orderIdCaption) ^
        MDC.filledTextField shortCaption # short ^
        MDC.filledTextField uniqueCaption # unique) # orderId ^
    div'
      ( MDC.subtitle1
        ( text # orderedByCaption) ^
        customer # orderedBy) ^
    div'
      ( MDC.checkbox paidCaption # paid) ^
    ( MDC.radioButton dineInCaption # isDineIn ^
      MDC.radioButton takeawayCaption # isTakeaway ^
      MDC.radioButton deliveryCaption # isDelivery) # fulfillment ^
    ( div'
        ( MDC.filledTextField tableCaption # table) # dineIn ^
      div'
        ( MDC.filledTextField timeCaption # time) # takeaway ^
      div'
        ( MDC.filledTextField addressCaption # address) # delivery) # fulfillment ^
    MDC.body1
      ( text # orderIdCaption ^
        chars " " ^
        text # short # orderId ^
        chars " (" ^
        text # unique # orderId ^
        chars "), " ^
        ( text # firstName ^
          chars " " ^
          text # lastName ^
            ( chars " (" ^
              text # surname ^
              chars " " ^
              text # forename ^
              chars ")") # formal) # orderedBy ^
      chars ", " ^
      text # paymentStatus # paid ^
      chars ", " ^
      text # fulfillmentData # fulfillment) ^
    div'
      ( MDC.containedButton (submitOrderCaption >>> shortId) submitOrder ))

customer :: Widget CustomerInformal CustomerInformal
customer =
  div'
    ( MDC.subtitle2
      ( text # informalCaption) ^
      MDC.filledTextField firstNameCaption # firstName ^
      MDC.filledTextField lastNameCaption # lastName) ^
  div'
    ( MDC.subtitle2
      ( text # formalCaption) ^
      MDC.filledTextField forenameCaption # forename ^
      MDC.filledTextField surnameCaption # surname) # formal
