module View
  ( order
  ) where

import Data.Function ((#), ($), (>>>))
import Data.Lens (_Just)
import Data.Semigroup ((<>))
import MDC (body1, card, checkbox, containedButton, dialog, elevation20, filledTextField, headline6, radioButton, snackbar, subtitle1, subtitle2)
import Model (Order, address, customer, delivery, dineIn, firstName, forename, formal, fulfillment, lastName, paid, payment, shortId, submitOrder, surname, table, takeaway, time, total, uniqueId)
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import Web (Web, div', slot, text)
import Widget (Widget, fixed)

order :: Widget Web Order Order
order =
  elevation20 S.do
    headline6 $ text # fixed "Order " <> shortId
    card S.do
      subtitle1 $ text # fixed "Identifier"
      filledTextField { floatingLabel: fixed "Short ID" } shortId
      filledTextField { floatingLabel: fixed "Unique ID" } uniqueId
    card ( S.do
      subtitle1 S.do
        text # fixed "Customer"
        subtitle2 $ text # fixed "Informal"
        filledTextField { floatingLabel: fixed "First name" } firstName
        filledTextField { floatingLabel: fixed "Last name" } lastName
        subtitle2 $ text # fixed "Formal"
        filledTextField { floatingLabel: fixed "Surname" } surname # formal
        filledTextField { floatingLabel: fixed "Forename" } forename # formal ) # customer
    card ( S.do
      radioButton { labelContent: text # fixed "Dine in", default: { table: "1"} } dineIn
      radioButton { labelContent: text # fixed "Takeaway", default: { time: "15:30"} } takeaway
      radioButton { labelContent: text # fixed "Delivery", default: { address: "Mulholland Drive 2001, Los Angeles" } } delivery
      filledTextField { floatingLabel: fixed "Table" } table # slot # dineIn
      filledTextField { floatingLabel: fixed "Time" } time # slot # takeaway
      filledTextField { floatingLabel: fixed "Address" } address # slot # delivery ) # fulfillment
    card S.do
      subtitle1 $ text # fixed "Total"
      filledTextField { floatingLabel: fixed "Total" } total
    card S.do
      checkbox { labelContent: text # fixed "Payment", default: { paid: "0" } } payment
      filledTextField { floatingLabel: fixed "Paid" } paid # slot # _Just # payment
    card S.do
      body1 $ text # fixed "Summary: Order " <> shortId <> fixed " (uniquely " <> uniqueId <> fixed ") for " <> firstName >>> customer <> fixed " " <> lastName >>> customer <> fixed " (formally " <> surname >>> formal >>> customer <> fixed " " <> forename >>> formal >>> customer <> fixed ")" <> fixed ", fulfilled as " <> (fixed "dine in at table " <> table) >>> slot >>> dineIn >>> fulfillment <> (fixed "takeaway at " <> time) >>> slot >>> takeaway >>> fulfillment <> (fixed "delivery to " <> address) >>> slot >>> delivery >>> fulfillment <> (fixed ", paid " <> paid) >>> slot >>> _Just >>> payment
      div' { style: "display: flex; justify-content: space-between; align-items: center; width: 100%;" } ( S.do
        containedButton { label: text # fixed "Submit order " <> shortId <> fixed " as draft" }
        containedButton { label: text # fixed "Submit order " <> shortId } ) >>> T.do
          dialog { title: text # fixed "Submit order " <> shortId <> fixed "?" } S.do
            body1 $ text # fixed "Are you sure?"
            containedButton { label: text # fixed "Submit order" }
          submitOrder
          snackbar { label: text # fixed "Order " <> shortId <> fixed " submitted"}
