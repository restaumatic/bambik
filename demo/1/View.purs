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
import Widget (Widget, affArr, constant)

order :: Widget Web Order Order
order =
  elevation20 S.do
    headline6 $ text # constant "Order " <> shortId
    card S.do
      subtitle1 $ text # constant "Identifier"
      filledTextField { floatingLabel: constant "Short ID" } shortId
      filledTextField { floatingLabel: constant "Unique ID" } uniqueId
    card ( S.do
      subtitle1 S.do
        text # constant "Customer"
        subtitle2 $ text # constant "Informal"
        filledTextField { floatingLabel: constant "First name" } firstName
        filledTextField { floatingLabel: constant "Last name" } lastName
        subtitle2 $ text # constant "Formal"
        filledTextField { floatingLabel: constant "Surname" } surname # formal
        filledTextField { floatingLabel: constant "Forename" } forename # formal ) # customer
    card ( S.do
      radioButton { labelContent: text # constant "Dine in", default: { table: "1"} } dineIn
      radioButton { labelContent: text # constant "Takeaway", default: { time: "15:30"} } takeaway
      radioButton { labelContent: text # constant "Delivery", default: { address: "Mulholland Drive 2001, Los Angeles" } } delivery
      filledTextField { floatingLabel: constant "Table" } table # slot # dineIn
      filledTextField { floatingLabel: constant "Time" } time # slot # takeaway
      filledTextField { floatingLabel: constant "Address" } address # slot # delivery ) # fulfillment
    card S.do
      subtitle1 $ text # constant "Total"
      filledTextField { floatingLabel: constant "Total" } total
    card S.do
      checkbox { labelContent: text # constant "Payment", default: { paid: "0" } } payment
      filledTextField { floatingLabel: constant "Paid" } paid # slot # _Just # payment
    card S.do
      body1 $ text # constant "Summary: Order " <> shortId <> constant " (uniquely " <> uniqueId <> constant ") for " <> firstName >>> customer <> constant " " <> lastName >>> customer <> constant " (formally " <> surname >>> formal >>> customer <> constant " " <> forename >>> formal >>> customer <> constant ")" <> constant ", fulfilled as " <> (constant "dine in at table " <> table) >>> slot >>> dineIn >>> fulfillment <> (constant "takeaway at " <> time) >>> slot >>> takeaway >>> fulfillment <> (constant "delivery to " <> address) >>> slot >>> delivery >>> fulfillment <> (constant ", paid " <> paid) >>> slot >>> _Just >>> payment
      div' { style: "display: flex; justify-content: space-between; align-items: center; width: 100%;" } ( S.do
        containedButton { label: text # constant "Submit order " <> shortId <> constant " as draft" }
        containedButton { label: text # constant "Submit order " <> shortId } ) >>> T.do
          dialog { title: text # constant "Submit order " <> shortId <> constant "?" } S.do
            body1 $ text # constant "Are you sure?"
            containedButton { label: text # constant "Submit order" }
          affArr submitOrder
          snackbar { label: text # constant "Order " <> shortId <> constant " submitted"}
