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
  ( MDC.headline6 (text # fixed "Order " ^ text # shortId)
  ^ MDC.card
    ( MDC.subtitle1 (text # fixed "Identifier")
    ^ MDC.filledTextField { caption: fixed "Short ID", value: shortId }
    ^ MDC.filledTextField { caption: fixed "Unique ID", value: uniqueId } )
  ^ MDC.card
    ( MDC.subtitle1 (text # fixed "Customer")
    ^ name # customer )
  ^ MDC.card
    ( MDC.radioButton { caption: text # fixed "Dine in", value: isDineIn }
    ^ MDC.radioButton { caption: text # fixed "Takeaway", value: isTakeaway }
    ^ MDC.radioButton { caption: text # fixed "Delivery", value: isDelivery }
    ^ MDC.filledTextField { caption: fixed "Table", value: table } # dineIn
    ^ MDC.filledTextField { caption: fixed "Time", value: time } # takeaway
    ^ MDC.filledTextField { caption: fixed "Address", value: address } # delivery ) # fulfillment
  ^ MDC.card
    ( MDC.subtitle1 (text # fixed "Total")
    ^ MDC.filledTextField { caption: fixed "Total", value: total } )
  ^ MDC.card
    ( MDC.checkbox { caption: \order2order -> total order2order ^ text # fixed " paid", checked: paid } )
  ^ MDC.card
    ( MDC.body1 orderSummary
    ^ MDC.containedButton (text # fixed "Submit order " ^ text # shortId )
      >>> MDC.dialog { title: text # fixed "Submit order?", content: ( MDC.body1 (fixed "Are you sure?") ^ MDC.containedButton (text # fixed "Submit order" ) ) }
      >>> serializeOrder
      >>> MDC.snackbar (\_ -> ( text # fixed "Order " ^ text ^ text # fixed " submitted" ))

       ) )

name :: Widget NameInformal NameInformal
name =
  MDC.subtitle2 (fixed "Informal")
  ^ MDC.filledTextField { caption: fixed "First name", value: firstName }
  ^ MDC.filledTextField { caption: fixed "Last name", value: lastName }
  ^ ( MDC.subtitle2 (fixed "Formal")
    ^ MDC.filledTextField { caption: fixed "Surname", value: surname }
    ^ MDC.filledTextField { caption: fixed "Forename", value: forename } ) # formal
