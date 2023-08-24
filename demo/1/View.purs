module View
  ( order
  ) where

import Prelude

import ViewModel
import Web
import Web.MDC as MDC

order ∷ Widget Order Order
order =
  MDC.elevation9
  ( MDC.headline4 orderTitle # shortId ^
    div'
      ( MDC.filledTextField shortIdCaption # shortId ) ^
    div'
      ( MDC.filledTextField uniqueIdCaption # uniqueId ) ^
    div'
      ( MDC.subtitle1 orderIdCaption ^
        MDC.filledTextField shortCaption # short ^
        MDC.filledTextField uniqueCaption # unique ) # orderId ^
    div'
      ( MDC.subtitle1 orderedByCaption ^
        customer # orderedBy ) ^
    div'
      ( MDC.checkbox paidCaption # paid ) ^
    ( MDC.radioButton dineInCaption # isDineIn ^
      MDC.radioButton takeawayCaption # isTakeaway ^
      MDC.radioButton deliveryCaption # isDelivery) # fulfillment ^
    ( div'
        ( MDC.filledTextField tableCaption # table # dineIn ) ^
      div'
        ( MDC.filledTextField timeCaption # time # takeaway ) ^
      div'
        ( MDC.filledTextField addressCaption # address # delivery ) ) # fulfillment ^
    MDC.card
      ( MDC.body1
        ( text # orderCaption ^
          chars " " ^
          text # short # orderId ^
          chars " (uniquely " ^
          text # unique # orderId ^
          chars ") for " ^
          ( text # firstName ^
            chars " " ^
            text # lastName ^
              ( chars " (formally " ^
                text # surname ^
                chars " " ^
                text # forename ^
                chars ")" ) # formal ) # orderedBy ^
        chars ", " ^
        text # paymentStatus # paid ^
        chars ", fulfilled as " ^
        text # fulfillmentData # fulfillment ) ^
        div'
          ( MDC.containedButton (submitOrderCaption >>> shortId) >>>
            ( MDC.dialog (submitOrderCaption >>> shortId)
              ( div'
                ( chars "Are you sure?" ) ^
                MDC.containedButton (submitOrderCaption >>> shortId) ) ) >>>
                  ( MDC.snackbar (orderSubmittedCaption >>> shortId))) ) )

customer :: Widget CustomerInformal CustomerInformal
customer =
  div'
    ( MDC.subtitle2 informalCaption ^
      MDC.filledTextField firstNameCaption # firstName ^
      MDC.filledTextField lastNameCaption # lastName) ^
  div'
    ( MDC.subtitle2 formalCaption ^
      MDC.filledTextField surnameCaption # surname ^
      MDC.filledTextField forenameCaption # forename ) # formal
