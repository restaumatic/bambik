module View
  ( order
  ) where

import Prelude hiding (div)

import Data.Lens (_Just)
import MDC as MDC
import QualifiedDo.Alt as A
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import ViewModel (NameInformal, Order, address, customer, delivery, dineIn, firstName, forename, formal, fulfillment, lastName, paid, payment, shortId, submitOrder, surname, table, takeaway, time, total, uniqueId)
import Web (Web, at', div, slot, text)
import Widget (Widget, fixed)

order :: Widget Web Order Order
order =
  MDC.elevation20 S.do
    MDC.headline6 S.do
      text # fixed "Order "
      text # shortId
    MDC.card S.do
      MDC.subtitle1 (text # fixed "Identifier")
      MDC.filledTextField { floatingLabel: (_ # fixed "Short ID") } shortId
      MDC.filledTextField { floatingLabel: (_ # fixed "Unique ID") } uniqueId
    MDC.card S.do
      MDC.subtitle1 S.do
        text # fixed "Customer"
        name # customer
    MDC.card ( S.do
      MDC.radioButton { labelContent: text # fixed "Dine in", default: { table: "1"} } dineIn
      MDC.radioButton { labelContent: text # fixed "Takeaway", default: { time: "15:30"} } takeaway
      MDC.radioButton { labelContent: text # fixed "Delivery", default: { address: "Mulholland Drive 2001, Los Angeles" } } delivery
      MDC.filledTextField { floatingLabel: (_ # fixed "Table" )} table # slot # dineIn
      MDC.filledTextField { floatingLabel: (_ # fixed "Time" )} time # slot # takeaway
      MDC.filledTextField { floatingLabel: (_ # fixed "Address" )} address # slot # delivery ) # fulfillment
    MDC.card S.do
      MDC.subtitle1 (text # fixed "Total")
      MDC.filledTextField { floatingLabel: (_ # fixed "Total" )} total
    MDC.card S.do
      MDC.checkbox { labelContent: text # fixed "Payment", default: { paid: "0" } } payment
      MDC.filledTextField { floatingLabel: (_ # fixed "Paid" )} paid # slot # _Just # payment
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
              text ) # table # slot # dineIn
            ( S.do
              text # fixed "takeaway at "
              text ) # time # slot # takeaway
            ( S.do
              text # fixed "delivery to "
              text ) # address # slot # delivery ) # fulfillment
        ( S.do
          text # fixed ", paid "
          text # paid ) # slot # _Just # payment
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
        submitOrder
        MDC.snackbar { label: S.do
          text # fixed "Order "
          text # shortId
          text # fixed " submitted"}

name :: Widget Web NameInformal NameInformal
name = S.do
  MDC.subtitle2 (text # fixed "Informal")
  MDC.filledTextField { floatingLabel: (_ # fixed "First name") } firstName
  MDC.filledTextField { floatingLabel: (_ # fixed "Last name") } lastName
  ( S.do
    MDC.subtitle2 (text # fixed "Formal")
    MDC.filledTextField { floatingLabel: (_ # fixed "Surname" )} surname
    MDC.filledTextField { floatingLabel: (_ # fixed "Forename" )} forename ) # formal
