module View
  ( order
  ) where

import Prelude
import ViewModel (NameInformal, Order, address, customer, delivery, dineIn, firstName, forename, formal, fulfillment, isDelivery, isDineIn, isTakeaway, lastName, paid, serializeOrder, shortId, surname, table, takeaway, time, total, uniqueId)
import Web (Widget, text, (^))

import Data.Profunctor.Change (fixed)
import Web.MDC as MDC

order ∷ Widget Order Order
order =
  MDC.elevation20
  ( MDC.headline6 (text # fixed "Order " ^ text # shortId)
  ^ MDC.card
    ( MDC.subtitle1 (text # fixed "Identifier")
    ^ MDC.filledTextField { floatingLabel: text # fixed "Short ID" } shortId
    ^ MDC.filledTextField { floatingLabel: text # fixed "Unique ID" } uniqueId )
  ^ MDC.card
    ( MDC.subtitle1 (text # fixed "Customer")
    ^ name # customer )
  ^ MDC.card
    ( MDC.radioButton { labelContent: text # fixed "Dine in" } isDineIn
    ^ MDC.radioButton { labelContent: text # fixed "Takeaway" } isTakeaway
    ^ MDC.radioButton { labelContent: text # fixed "Delivery" } isDelivery
    ^ MDC.filledTextField { floatingLabel: text # fixed "Table" } table # dineIn
    ^ MDC.filledTextField { floatingLabel: text # fixed "Time" } time # takeaway
    ^ MDC.filledTextField { floatingLabel: text # fixed "Address" } address # delivery ) # fulfillment
  ^ MDC.card
    ( MDC.subtitle1 (text # fixed "Total")
    ^ MDC.filledTextField { floatingLabel: text # fixed "Total" } total )
  ^ MDC.card
    ( MDC.checkbox { labelContent: text # total ^ text # fixed " paid" } paid )
  ^ MDC.card
    ( MDC.body1
      ( text # fixed "Summary: Order "
      ^ text # shortId
      ^ text # fixed " (uniquely "
      ^ text # uniqueId
      ^ text # fixed ") for "
      ^ ( text # firstName
        ^ text # fixed " "
        ^ text # lastName
          ^ ( text # fixed " (formally "
            ^ text # surname
            ^ text # fixed " "
            ^ text # forename
            ^ text # fixed ")" ) # formal ) # customer
      ^ text # fixed ", fulfilled as "
      ^ ( (text # fixed "dine in at table " ^ text) # table # dineIn
        ^ (text # fixed "takeaway at " ^ text) # time # takeaway
        ^ (text # fixed "delivery to " ^ text) # address # delivery ) # fulfillment )
    ^ MDC.containedButton { label: text # fixed "Submit order " ^ text # shortId }
      >>> MDC.dialog { title: text # fixed "Submit order " ^ text # shortId ^ text # fixed "?"}
          ( MDC.body1 (text # fixed "Are you sure?")
          ^ MDC.containedButton { label: text # fixed "Submit order" } )
      >>> serializeOrder
      >>> MDC.snackbar { label: text # fixed "Order \"" ^ text ^ text # fixed "\" submitted" } ) )

name :: Widget NameInformal NameInformal
name =
  MDC.subtitle2 (text # fixed "Informal")
  ^ MDC.filledTextField { floatingLabel: text # fixed "First name" } firstName
  ^ MDC.filledTextField { floatingLabel: text # fixed "Last name" } lastName
  ^ ( MDC.subtitle2 (text # fixed "Formal")
    ^ MDC.filledTextField { floatingLabel: text # fixed "Surname" } surname
    ^ MDC.filledTextField { floatingLabel: text # fixed "Forename" } forename ) # formal
