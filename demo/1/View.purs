module View
  ( order
  ) where

import Prelude
import ViewModel
import Web

import Web.MDC as MDC

order ∷ Widget Order Order
order =
  MDC.elevation20
  ( MDC.headline6 orderTitle
  ^ MDC.card
    ( MDC.subtitle1 idCaption
    ^ MDC.filledTextField shortIdCaption # shortId
    ^ MDC.filledTextField uniqueIdCaption # uniqueId )
  ^ MDC.card
    ( MDC.subtitle1 customerCaption
    ^ name # customer )
  ^ MDC.card
    ( MDC.checkbox paidCaption # paid )
  ^ MDC.card
    ( MDC.radioButton dineInCaption # isDineIn
    ^ MDC.radioButton takeawayCaption # isTakeaway
    ^ MDC.radioButton deliveryCaption # isDelivery
    ^ MDC.filledTextField tableCaption # table # dineIn
    ^ MDC.filledTextField timeCaption # time # takeaway
    ^ MDC.filledTextField addressCaption # address # delivery ) # fulfillment
  ^ MDC.card
    ( MDC.body1 orderSummary
    ^ MDC.containedButton submitOrderCaption
      >>> MDC.dialog submitOrderCaption
          ( MDC.body1 areYouSureText
          ^ MDC.containedButton submitOrderCaption )
      >>> MDC.snackbar orderSubmittedCaption ) )

name :: Widget NameInformal NameInformal
name =
  ( MDC.subtitle2 informalCaption
  ^ MDC.filledTextField firstNameCaption # firstName
  ^ MDC.filledTextField lastNameCaption # lastName )
  ^ ( MDC.subtitle2 formalCaption
    ^ MDC.filledTextField surnameCaption # surname
    ^ MDC.filledTextField forenameCaption # forename ) # formal
