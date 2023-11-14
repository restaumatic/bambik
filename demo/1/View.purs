module View
  ( order
  ) where

import Prelude hiding (div)

import Data.Lens (_Just)
import Propagator (debounced', fixed, precededByEffect, empty)
import QualifiedDo.Alt as A
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import ViewModel (NameInformal, Order, address, customer, delivery, dineIn, firstName, forename, formal, fulfillment, hasPayment, isDelivery, isDineIn, isTakeaway, lastName, paid, payment, shortId, submitOrder, surname, table, takeaway, time, total, uniqueId)
import Web (Widget, div, text)
import Web.Internal.DOM (attr)
import Web.MDC as MDC

order ∷ Widget Order Order
order =
  MDC.elevation20 S.do
    MDC.headline6 S.do
      text # fixed "Order "
      text # shortId
    MDC.card S.do
      MDC.subtitle1 (text # fixed "Identifier")
      MDC.filledTextField { floatingLabel: text # fixed "Short ID" } shortId # debounced'
      MDC.filledTextField { floatingLabel: text # fixed "Unique ID" } uniqueId # debounced'
    MDC.card S.do
      MDC.subtitle1 S.do
        text # fixed "Customer"
        name # customer
    MDC.card ( S.do
      MDC.radioButton { labelContent: text # fixed "Dine in" } isDineIn
      MDC.radioButton { labelContent: text # fixed "Takeaway" } isTakeaway
      MDC.radioButton { labelContent: text # fixed "Delivery" } isDelivery
      MDC.filledTextField { floatingLabel: text # fixed "Table" } table # debounced' # dineIn
      MDC.filledTextField { floatingLabel: text # fixed "Time" } time # debounced' # takeaway
      MDC.filledTextField { floatingLabel: text # fixed "Address" } address # debounced' # delivery ) # fulfillment
    MDC.card S.do
      MDC.subtitle1 (text # fixed "Total")
      MDC.filledTextField { floatingLabel: text # fixed "Total" } total # debounced'
    MDC.card S.do
      MDC.checkbox { labelContent: S.do
        text # fixed "Payment" } hasPayment # payment
      MDC.filledTextField { floatingLabel: text # fixed "Paid" } paid # _Just # payment
    MDC.card S.do
      MDC.body1 S.do
        text # fixed "Summary: Order "
        text # shortId
        text # fixed " (uniquely "
        text # uniqueId
        text # fixed ") for "
        ( S.do
          text # firstName
          text # fixed " "
          text # lastName
          ( S.do
            text # fixed " (formally "
            text # surname
            text # fixed " "
            text # forename
            text # fixed ")" ) # formal ) # customer
        text # fixed ", fulfilled as "
        ( S.do
            ( S.do
              text # fixed "dine in at table "
              text ) # table # dineIn
            ( S.do
              text # fixed "takeaway at "
              text) # time # takeaway
            ( S.do
              text # fixed "delivery to "
              text) # address # delivery ) # fulfillment
      T.do
        div (attr "style" "display: flex; justify-content: space-between; align-items: center; width: 100%;") mempty
          A.do
            MDC.containedButton { label: S.do
              text # fixed "Submit order "
              text # shortId
              text # fixed " as draft" }
            MDC.containedButton { label: S.do
              text # fixed "Submit order "
              text # shortId }
        MDC.dialog { title: S.do
          text # fixed "Submit order "
          text # shortId
          text # fixed "?" } S.do
            MDC.body1 (text # fixed "Are you sure?")
            MDC.containedButton { label: text # fixed "Submit order" }
        MDC.snackbar { label: S.do
          text # fixed "Order "
          text # shortId
          text # fixed " submitted"} # precededByEffect submitOrder
        empty

name :: Widget NameInformal NameInformal
name = S.do
  MDC.subtitle2 (text # fixed "Informal")
  MDC.filledTextField { floatingLabel: text # fixed "First name" } firstName # debounced'
  MDC.filledTextField { floatingLabel: text # fixed "Last name" } lastName # debounced'
  ( S.do
    MDC.subtitle2 (text # fixed "Formal")
    MDC.filledTextField { floatingLabel: text # fixed "Surname" } surname # debounced'
    MDC.filledTextField { floatingLabel: text # fixed "Forename" } forename # debounced' ) # formal
