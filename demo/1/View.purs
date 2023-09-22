module View
  ( order
  ) where

import Prelude
import ViewModel
import Web

import Data.Profunctor.Change (fixed) -- TODO: replace fixed with purePP (pure product profunctor)?
import Web.MDC as MDC

order ∷ Widget Order Order
order =
  MDC.elevation20
  ( MDC.headline6 orderTitle
  ^ MDC.card
    ( MDC.subtitle1 (fixed "Identifier")
    ^ MDC.filledTextField { caption: fixed "Short ID", value: shortId }
    ^ MDC.filledTextField { caption: fixed "Unique ID", value: uniqueId } )
  ^ MDC.card
    ( MDC.subtitle1 (fixed "Customer")
    ^ name # customer )
  ^ MDC.card
    ( MDC.checkbox { caption: fixed "Paid", checked: paid } )
  ^ MDC.card
    ( MDC.radioButton { caption: fixed "Dine in", value: isDineIn }
    ^ MDC.radioButton { caption: fixed "Takeaway", value: isTakeaway }
    ^ MDC.radioButton { caption: fixed "Delivery", value: isDelivery }
    ^ MDC.filledTextField { caption: fixed "Table", value: table } # dineIn
    ^ MDC.filledTextField { caption: fixed "Time", value: time } # takeaway
    ^ MDC.filledTextField { caption: fixed "Address", value: address } # delivery ) # fulfillment
  ^ MDC.card
    ( MDC.body1 orderSummary
    ^ MDC.containedButton submitOrderCaption
      >>> MDC.dialog { title: submitOrderCaption, content: identity}
          ( MDC.body1 (fixed "Are you sure?")
          ^ MDC.containedButton submitOrderCaption )
      >>> serializeOrder
      >>> MDC.snackbar orderSubmittedCaption ) )

name :: Widget NameInformal NameInformal
name =
  MDC.subtitle2 (fixed "Informal")
  ^ MDC.filledTextField { caption: fixed "First name", value: firstName }
  ^ MDC.filledTextField { caption: fixed "Last name", value: lastName }
  ^ ( MDC.subtitle2 (fixed "Formal")
    ^ MDC.filledTextField { caption: fixed "Surname", value: surname }
    ^ MDC.filledTextField { caption: fixed "Forename", value: forename } ) # formal
