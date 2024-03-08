module View
  ( order
  ) where

import Data.Profunctor (lcmap)
import MDC (body1, caption, card, checkbox, confirmationDialog, containedButton, elevation20, filledTextField, indeterminateLinearProgress, radioButton, snackbar)
import Model (Order, OrderId, address, authToken, customer, delivery, dineIn, firstName, forename, formal, fulfillment, lastName, loadOrder, orderId, paid, payment, shortId, submitOrder, submittedOrder, surname, table, takeaway, time, total)
import Prelude ((#), ($), (<>), (>>>))
import QualifiedDo.SumProfunctor as S
import QualifiedDo.Semigroupoid as T
import Web (Web, slot, text)
import Widget (Widget, constant, debouncer', just, spy, value)

order :: Widget Web Order Order
order = -- T.do
  elevation20 $ S.do
    caption $ text # constant "Order " <> value >>> debouncer' >>> shortId
    card S.do
      caption $ text # constant "Identifier"
      filledTextField { floatingLabel: constant "Short ID" } (spy "shortid") # shortId # spy "order!"
      filledTextField { floatingLabel: constant "Unique ID" } orderId
    card >>> customer $ S.do
      caption $ text # constant "Customer"
      caption $ text # constant "Informal"
      filledTextField { floatingLabel: constant "First name" } firstName
      filledTextField { floatingLabel: constant "Last name" } lastName
      caption $ text # constant "Formal"
      filledTextField { floatingLabel: constant "Surname" } surname # formal
      filledTextField { floatingLabel: constant "Forename" } forename # formal
    card >>> fulfillment $ S.do
      caption $ text # constant "Fulfillment"
      radioButton { labelContent: text # constant "Dine in", default: { table: "1"} } dineIn
      radioButton { labelContent: text # constant "Takeaway", default: { time: "15:30"} } takeaway
      radioButton { labelContent: text # constant "Delivery", default: { address: "Mulholland Drive 2001, Los Angeles" } } delivery
      S.do
        filledTextField { floatingLabel: constant "Table" } table # slot # dineIn
        filledTextField { floatingLabel: constant "Time" } time # slot # takeaway
        filledTextField { floatingLabel: constant "Address" } address # slot # delivery
    card S.do
      caption $ text # constant "Payment"
      filledTextField { floatingLabel: constant "Total" } total
      S.do
        checkbox { labelContent: text # constant "Paid", default: { paid: "0" } } payment
        filledTextField { floatingLabel: constant "Paid" } paid # just # payment
    card S.do
      body1 $ text # constant "Order " <> value >>> shortId <> constant " (uniquely " <> value >>> orderId <> constant ") for " <> value >>> firstName >>> customer <> constant " " <> value >>> lastName >>> customer <> constant " (formally " <> value >>> surname >>> formal >>> customer <> constant " " <> value >>> forename >>> formal >>> customer <> constant ")" <> constant ", fulfilled as " <> (constant "dine in at table " <> value >>> table) >>> slot >>> dineIn >>> fulfillment <> (constant "takeaway at " <> value >>> time) >>> slot >>> takeaway >>> fulfillment <> (constant "delivery to " <> value >>> address) >>> slot >>> delivery >>> fulfillment <> (constant ", paid " <> value >>> paid) >>> slot >>> just >>> payment # slot # debouncer'
  -- containedButton { label: text # constant "Submit order " <> value >>> shortId >>> debouncer' }
  -- confirmationDialog { title: text # constant "Submit order " <> value >>> shortId >>> submittedOrder <> constant "?", dismiss: text # constant "No", confirm: text # constant "Yes" } >>> lcmap (\submittedOrder -> { authToken: "", submittedOrder }) $ S.do
  --   body1 $ text # constant "Authorization required"
  --   filledTextField { floatingLabel: constant "Auth token" } authToken
  -- indeterminateLinearProgress # submitOrder
  -- snackbar { label: text # constant "Order " <> value >>> shortId <> constant " submitted"}
