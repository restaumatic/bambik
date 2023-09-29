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
    ^ MDC.filledTextField { caption: fixed "Short ID" } shortId
    ^ MDC.filledTextField { caption: fixed "Unique ID" } uniqueId )
  ^ MDC.card
    ( MDC.subtitle1 (text # fixed "Customer")
    ^ name # customer )
  ^ MDC.card
    ( MDC.radioButton { caption: fixed "Dine in" } isDineIn
    ^ MDC.radioButton { caption: fixed "Takeaway" } isTakeaway
    ^ MDC.radioButton { caption: fixed "Delivery" } isDelivery 
    ^ MDC.filledTextField { caption: fixed "Table" } table # dineIn
    ^ MDC.filledTextField { caption: fixed "Time" } time # takeaway
    ^ MDC.filledTextField { caption: fixed "Address" } address # delivery ) # fulfillment
  ^ MDC.card
    ( MDC.subtitle1 (text # fixed "Total")
    ^ MDC.filledTextField { caption: fixed "Total" } total )
  ^ MDC.card
    ( MDC.checkbox { caption: \cap -> (cap # total) ^ text # fixed " paid" } paid )
  ^ MDC.card
    ( MDC.body1 orderSummary
    ^ MDC.containedButton (text # fixed "Submit order " ^ text # shortId )
      >>> MDC.dialog { title: text # fixed "Submit order " ^ text # shortId ^ text # fixed "?"}
        ( MDC.body1 (fixed "Are you sure?")
        ^ MDC.containedButton (text # fixed "Submit order" ) )
      >>> serializeOrder
      >>> MDC.snackbar (\_ -> ( text # fixed "Order " ^ text ^ text # fixed " submitted" ) ) ) )

name :: Widget NameInformal NameInformal
name =
  MDC.subtitle2 (fixed "Informal")
  ^ MDC.filledTextField { caption: fixed "First name" } firstName
  ^ MDC.filledTextField { caption: fixed "Last name" } lastName
  ^ ( MDC.subtitle2 (fixed "Formal")
    ^ MDC.filledTextField { caption: fixed "Surname" } surname
    ^ MDC.filledTextField { caption: fixed "Forename" } forename ) # formal
