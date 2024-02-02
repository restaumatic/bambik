module View
  ( order
  ) where


-- commons

import Data.Function ((#))
import Data.Lens (_Just)
import Data.Semigroup ((<>))
import MDC (body1, card, checkbox, containedButton, dialog, elevation20, filledTextField, headline6, radioButton, snackbar, subtitle1, subtitle2)
import Model (NameInformal, Order, address, customer, delivery, dineIn, firstName, forename, formal, fulfillment, lastName, paid, payment, shortId, submitOrder, surname, table, takeaway, time, total, uniqueId)
import QualifiedDo.Alt as A
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import Web (Web, attr, div, slot, text)
import Widget (Widget, fixed)

order :: Widget Web Order Order
order =
  elevation20 S.do
    headline6 (text # fixed "Order " <> shortId)
    card S.do
      subtitle1 (text # fixed "Identifier")
      filledTextField { floatingLabel: (_ # fixed "Short ID") } shortId
      filledTextField { floatingLabel: (_ # fixed "Unique ID") } uniqueId
    card S.do
      subtitle1 S.do
        text # fixed "Customer"
        name # customer
    card ( S.do
      radioButton { labelContent: text # fixed "Dine in", default: { table: "1"} } dineIn
      radioButton { labelContent: text # fixed "Takeaway", default: { time: "15:30"} } takeaway
      radioButton { labelContent: text # fixed "Delivery", default: { address: "Mulholland Drive 2001, Los Angeles" } } delivery
      filledTextField { floatingLabel: (_ # fixed "Table" )} table # slot # dineIn
      filledTextField { floatingLabel: (_ # fixed "Time" )} time # slot # takeaway
      filledTextField { floatingLabel: (_ # fixed "Address" )} address # slot # delivery ) # fulfillment
    card S.do
      subtitle1 (text # fixed "Total")
      filledTextField { floatingLabel: (_ # fixed "Total" )} total
    card S.do
      checkbox { labelContent: text # fixed "Payment", default: { paid: "0" } } payment
      filledTextField { floatingLabel: (_ # fixed "Paid" )} paid # slot # _Just # payment
    card S.do
      body1 S.do
        text # fixed "Summary: Order " <> shortId <> fixed " (uniquely " <> uniqueId <> fixed ") for "
        ( S.do
          text # firstName <> fixed " " <> lastName
          ( text # fixed " (formally " <> surname <> fixed " " <> forename <> fixed ")" ) # formal ) # customer
        text # fixed ", fulfilled as "
        ( S.do
            text # fixed "dine in at table " <> table # slot # dineIn
            text # fixed "takeaway at " <> time # slot # takeaway
            text # fixed "delivery to " <> address # slot # delivery ) # fulfillment
        text # fixed ", paid " <> paid # slot # _Just # payment
      T.do
        div
          (A.do
            containedButton { label: text # fixed "Submit order " <> shortId <> fixed " as draft" }
            containedButton { label: text # fixed "Submit order " <> shortId }) # attr "style" "display: flex; justify-content: space-between; align-items: center; width: 100%;"
        dialog { title: text # fixed "Submit order " <> shortId <> fixed "?" } S.do
            body1 (text # fixed "Are you sure?")
            containedButton { label: text # fixed "Submit order" }
        submitOrder
        snackbar { label: text # fixed "Order " <> shortId <> fixed " submitted"}

name :: Widget Web NameInformal NameInformal
name = S.do
  subtitle2 (text # fixed "Informal")
  filledTextField { floatingLabel: (_ # fixed "First name") } firstName
  filledTextField { floatingLabel: (_ # fixed "Last name") } lastName
  ( S.do
    subtitle2 (text # fixed "Formal")
    filledTextField { floatingLabel: (_ # fixed "Surname" )} surname
    filledTextField { floatingLabel: (_ # fixed "Forename" )} forename ) # formal
