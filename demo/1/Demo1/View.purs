module Demo1.View
  ( order
  ) where

-- standard stuff
import Prelude
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T

-- framework
import Data.Profunctor (lcmap)
import MDC (body1, caption, card, checkbox, confirmationDialog, containedButton, elevation20, filledTextField, indeterminateLinearProgress, radioButton, snackbar)
import Web (Web, slot, text)
import Widget (Widget, debouncer', just, static, value)

-- model
import Demo1.Model (Order, OrderId, address, authToken, customer, delivery, dineIn, firstName, forename, formal, fulfillment, lastName, loadOrder, orderId, paid, payment, shortId, submitOrder, surname, table, takeaway, time, total)


order :: Widget Web OrderId Order
order = T.do
  indeterminateLinearProgress # loadOrder
  elevation20 $ S.do
    caption $ text # static "Order " <> value >>> debouncer' >>> shortId
    card $ S.do
      caption $ text # static "Identifier"
      filledTextField { floatingLabel: "Short ID" } # shortId
      filledTextField { floatingLabel: "Unique ID" } # orderId
    card >>> customer $ S.do
      caption $ text # static "Customer"
      caption $ text # static "Informal"
      filledTextField { floatingLabel: "First name" } # firstName
      filledTextField { floatingLabel: "Last name" } # lastName
      caption $ text # static "Formal"
      filledTextField { floatingLabel: "Surname" } # surname >>> formal
      filledTextField { floatingLabel: "Forename" } # forename >>> formal
    card >>> fulfillment $ S.do
      caption $ text # static "Fulfillment"
      radioButton { labelContent: "Dine in", default: { table: "1"} } # dineIn
      radioButton { labelContent: "Takeaway", default: { time: "15:30"} } # takeaway
      radioButton { labelContent: "Delivery", default: { address: "Mulholland Drive 2001, Los Angeles" } } # delivery
      filledTextField { floatingLabel: "Table" } # table >>> slot >>> dineIn
      filledTextField { floatingLabel: "Time" } # time >>> slot >>> takeaway
      filledTextField { floatingLabel: "Address" } # address >>> slot >>> delivery
    card $ S.do
      caption $ text # static "Payment"
      filledTextField { floatingLabel: "Total" } # total
      checkbox { labelContent: "Paid", default: { paid: "0" } } # payment
      filledTextField { floatingLabel: "Paid" } # paid >>> just >>> payment
    card $ S.do
      body1 $ text # (S.do
        static "Order "
        value >>> shortId
        static " (uniquely "
        value >>> orderId
        static ") for "
        value >>> firstName >>> customer
        static " "
        value >>> lastName >>> customer
        static " (formally "
        value >>> surname >>> formal >>> customer
        static " "
        value >>> forename >>> formal >>> customer
        static "), fulfilled as "
        (S.do
          static "dine in at table "
          value >>> table) >>> slot >>> dineIn >>> fulfillment
        (S.do
          static "takeaway at "
          value >>> time) >>> slot >>> takeaway >>> fulfillment
        (S.do
          static "delivery to "
          value >>> address) >>> slot >>> delivery >>> fulfillment
        (S.do
          static ", paid "
          value >>> paid) >>> slot >>> just >>> payment) >>> slot >>> debouncer'
  T.do
    containedButton { label: static "Submit order " <> value >>> shortId >>> debouncer' }
    confirmationDialog { title: "Submit order", dismiss: "No", confirm: "Yes" } >>> lcmap (\submittedOrder -> { authToken: "", submittedOrder }) $ S.do
      body1 $ text # static "Authorization required"
      filledTextField { floatingLabel: "Auth token" } # authToken
    indeterminateLinearProgress # submitOrder
    -- snackbar { label: static "Order " <> value >>> shortId <> static " submitted"}
