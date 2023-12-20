module View
  ( order
  ) where

import Prelude hiding (div)

import Data.Lens (_Just)
import Propagator (debounce', fixed, effect, empty)
import QualifiedDo.Alt as A
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import ViewModel (NameInformal, Order, address, customer, delivery, dineIn, firstName, forename, formal, fulfillment, hasPayment, isDelivery, isDineIn, isTakeaway, lastName, paid, payment, shortId, submitOrder, surname, table, takeaway, time, total, uniqueId)
import Web (Widget, at', div, text)
import Web.MDC as MDC

order ∷ Widget Order Order
order =
  MDC.elevation20 S.do
    MDC.headline6 S.do
      text # fixed "Order "
      text # shortId
    MDC.card S.do
      MDC.subtitle1 (text # fixed "Identifier")
      MDC.filledTextField { floatingLabel: (_ # fixed "Short ID") } shortId >>> debounce'
      MDC.filledTextField { floatingLabel: (_ # fixed "Unique ID") } uniqueId >>> debounce'
    MDC.card S.do
      MDC.subtitle1 S.do
        text # fixed "Customer"
        name # customer
    MDC.card ( S.do
      MDC.radioButton { labelContent: text # fixed "Dine in" } isDineIn
      MDC.radioButton { labelContent: text # fixed "Takeaway" } isTakeaway
      MDC.radioButton { labelContent: text # fixed "Delivery" } isDelivery
      MDC.filledTextField { floatingLabel: (_ # fixed "Table" )} table >>> debounce' # dineIn
      MDC.filledTextField { floatingLabel: (_ # fixed "Time" )} time >>> debounce' # takeaway
      MDC.filledTextField { floatingLabel: (_ # fixed "Address" )} address >>> debounce' # delivery ) # fulfillment
    MDC.card S.do
      MDC.subtitle1 (text # fixed "Total")
      MDC.filledTextField { floatingLabel: (_ # fixed "Total" )} total >>> debounce'
    MDC.card S.do
      MDC.checkbox { labelContent: text # fixed "Payment" } hasPayment # payment
      MDC.filledTextField { floatingLabel: (_ # fixed "Paid" )} paid # _Just # payment
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
        div
          (A.do
            MDC.containedButton { label: S.do
              text # fixed "Submit order "
              text # shortId
              text # fixed " as draft" }
            MDC.containedButton { label: S.do
              text # fixed "Submit order "
              text # shortId }) # at' "style" "display: flex; justify-content: space-between; align-items: center; width: 100%;"
        MDC.dialog { title: S.do
          text # fixed "Submit order "
          text # shortId
          text # fixed "?" } S.do
            MDC.body1 (text # fixed "Are you sure?")
            MDC.containedButton { label: text # fixed "Submit order" }
        effect submitOrder >>> MDC.snackbar { label: S.do
          text # fixed "Order "
          text # shortId
          text # fixed " submitted"}
        empty

name :: Widget NameInformal NameInformal
name = S.do
  MDC.subtitle2 (text # fixed "Informal")
  MDC.filledTextField { floatingLabel: (_ # fixed "First name") } firstName >>> debounce'
  MDC.filledTextField { floatingLabel: (_ # fixed "Last name") } lastName >>> debounce'
  ( S.do
    MDC.subtitle2 (text # fixed "Formal")
    MDC.filledTextField { floatingLabel: (_ # fixed "Surname" )} surname >>> debounce'
    MDC.filledTextField { floatingLabel: (_ # fixed "Forename" )} forename >>> debounce' ) # formal
