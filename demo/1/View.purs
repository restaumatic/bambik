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
  ( MDC.headline4 orderTitle
  ^ MDC.filledTextField shortIdCaption # shortId
  ^ MDC.filledTextField uniqueIdCaption # uniqueId
  ^ ( MDC.subtitle1 orderIdCaption
    ^ MDC.filledTextField shortCaption # short
    ^ MDC.filledTextField uniqueCaption # unique ) # orderId
  ^ ( MDC.subtitle1 orderedByCaption
    ^ customer # orderedBy )
  ^ ( MDC.checkbox paidCaption # paid )
  ^ ( MDC.radioButton dineInCaption # isDineIn
    ^ MDC.radioButton takeawayCaption # isTakeaway
    ^ MDC.radioButton deliveryCaption # isDelivery) # fulfillment
  ^ div'
    ( MDC.filledTextField tableCaption # table # dineIn
    ^ MDC.filledTextField timeCaption # time # takeaway
    ^ MDC.filledTextField addressCaption # address # delivery ) # fulfillment
  ^ MDC.card
    ( MDC.body1 orderSummary
    ^ div'
      ( MDC.containedButton submitOrderCaption
      >>> MDC.dialog submitOrderCaption
          ( MDC.body1 areYouSureText
          ^ MDC.containedButton submitOrderCaption )
      >>> MDC.snackbar 2000.0 orderSubmittedCaption ) ) )

customer :: Widget CustomerInformal CustomerInformal
customer =
  div'
    ( MDC.subtitle2 informalCaption
    ^ MDC.filledTextField firstNameCaption # firstName
    ^ MDC.filledTextField lastNameCaption # lastName )
  ^ div'
    ( MDC.subtitle2 formalCaption
    ^ MDC.filledTextField surnameCaption # surname
    ^ MDC.filledTextField forenameCaption # forename ) # formal
