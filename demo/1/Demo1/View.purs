module Demo1.View
  ( order
  ) where

import Data.Profunctor (lcmap)
import MDC (body1, caption, card, checkbox, confirmationDialog, containedButton, elevation20, filledTextField, indeterminateLinearProgress, radioButton, snackbar)
import Demo1.Model (Order, OrderId, address, authToken, customer, delivery, dineIn, firstName, forename, formal, fulfillment, lastName, loadOrder, orderId, paid, payment, shortId, submitOrder, submittedOrder, surname, table, takeaway, time, total)
import Prelude ((#), ($), (<>), (>>>))
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import Web (Web, slot, text)
import Widget (Widget, static, debouncer', just, value)

order :: Widget Web OrderId Order
order = T.do
  indeterminateLinearProgress # loadOrder
  elevation20 $ S.do
    caption $ text # static "Order " <> value >>> debouncer' >>> shortId
    card $ S.do
      caption $ text # static "Identifier"
      filledTextField { floatingLabel: static "Short ID" } shortId
      filledTextField { floatingLabel: static "Unique ID" } orderId
    card >>> customer $ S.do
      caption $ text # static "Customer"
      caption $ text # static "Informal"
      filledTextField { floatingLabel: static "First name" } firstName
      filledTextField { floatingLabel: static "Last name" } lastName
      caption $ text # static "Formal"
      filledTextField { floatingLabel: static "Surname" } surname # formal
      filledTextField { floatingLabel: static "Forename" } forename # formal
    card >>> fulfillment $ S.do
      caption $ text # static "Fulfillment"
      radioButton { labelContent: static "Dine in", default: { table: "1"} } dineIn
      radioButton { labelContent: static "Takeaway", default: { time: "15:30"} } takeaway
      radioButton { labelContent: static "Delivery", default: { address: "Mulholland Drive 2001, Los Angeles" } } delivery
      filledTextField { floatingLabel: static "Table" } table # slot >>> dineIn
      filledTextField { floatingLabel: static "Time" } time # slot >>> takeaway
      filledTextField { floatingLabel: static "Address" } address # slot >>> delivery
    card $ S.do
      caption $ text # static "Payment"
      filledTextField { floatingLabel: static "Total" } total
      checkbox { labelContent: static "Paid", default: { paid: "0" } } payment
      filledTextField { floatingLabel: static "Paid" } paid # just >>> payment
    card $ S.do
      body1 $ text # static "Order " <> value >>> shortId <> static " (uniquely " <> value >>> orderId <> static ") for " <> value >>> firstName >>> customer <> static " " <> value >>> lastName >>> customer <> static " (formally " <> value >>> surname >>> formal >>> customer <> static " " <> value >>> forename >>> formal >>> customer <> static ")" <> static ", fulfilled as " <> (static "dine in at table " <> value >>> table) >>> slot >>> dineIn >>> fulfillment <> (static "takeaway at " <> value >>> time) >>> slot >>> takeaway >>> fulfillment <> (static "delivery to " <> value >>> address) >>> slot >>> delivery >>> fulfillment <> (static ", paid " <> value >>> paid) >>> slot >>> just >>> payment # slot # debouncer'
      T.do
        containedButton { label: static "Submit order " <> value >>> shortId >>> debouncer' }
        -- confirmationDialog { title: static "Submit order " <> value >>> shortId >>> submittedOrder <> static "?", dismiss: static "No", confirm: static "Yes" } >>> lcmap (\submittedOrder -> { authToken: "", submittedOrder }) $ S.do
        --   body1 $ text # static "Authorization required"
        --   filledTextField { floatingLabel: static "Auth token" } authToken
        -- indeterminateLinearProgress # submitOrder
        -- snackbar { label: static "Order " <> value >>> shortId <> static " submitted"}
