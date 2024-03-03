module View
  ( order
  ) where

import Prelude

import Data.Profunctor (lcmap)
import Data.Profunctor.Strong (first, second)
import Data.Tuple (Tuple(..))
import MDC (body1, card, checkbox, containedButton, confirmationDialog, elevation20, filledTextField, headline6, indeterminateLinearProgress, radioButton, snackbar, subtitle1, subtitle2)
import Model (Order, OrderId, address, customer, delivery, dineIn, firstName, forename, formal, fulfillment, lastName, loadOrder, orderId, paid, payment, shortId, submitOrder, surname, table, takeaway, time, total)
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import Web (Web, slot, text)
import Widget (Widget, constant, debouncer', just, value)

order :: Widget Web OrderId Order
order = (indeterminateLinearProgress # loadOrder) >>> S.do
  elevation20 $ S.do
    headline6 $ S.do
      text # constant "Order " <> value >>> shortId
    card S.do
      subtitle1 $
        text # constant "Identifier"
      filledTextField { floatingLabel: constant "Short ID" } shortId
      filledTextField { floatingLabel: constant "Unique ID" } orderId
    card >>> customer $ S.do
      subtitle1 $
        text # constant "Customer"
      subtitle2 $
        text # constant "Informal"
      filledTextField { floatingLabel: constant "First name" } firstName
      filledTextField { floatingLabel: constant "Last name" } lastName
      subtitle2 $
        text # constant "Formal"
      filledTextField { floatingLabel: constant "Surname" } surname # formal
      filledTextField { floatingLabel: constant "Forename" } forename # formal
    card >>> fulfillment $ S.do
      radioButton { labelContent: text # constant "Dine in", default: { table: "1"} } dineIn
      radioButton { labelContent: text # constant "Takeaway", default: { time: "15:30"} } takeaway
      radioButton { labelContent: text # constant "Delivery", default: { address: "Mulholland Drive 2001, Los Angeles" } } delivery
      filledTextField { floatingLabel: constant "Table" } table # dineIn
      filledTextField { floatingLabel: constant "Time" } time # takeaway
      filledTextField { floatingLabel: constant "Address" } address # delivery
    card $ S.do
      subtitle1 $
        text # constant "Total"
      filledTextField { floatingLabel: constant "Total" } total
    card $ S.do
      checkbox { labelContent: text # constant "Payment", default: { paid: "0" } } payment
      filledTextField { floatingLabel: constant "Paid" } paid # just # payment
    card $ S.do
      body1 $
        text # constant "Summary: Order " <> value >>> shortId <> constant " (uniquely " <> value >>> orderId <> constant ") for " <> value >>> firstName >>> customer <> constant " " <> value >>> lastName >>> customer <> constant " (formally " <> value >>> surname >>> formal >>> customer <> constant " " <> value >>> forename >>> formal >>> customer <> constant ")" <> constant ", fulfilled as " <> (constant "dine in at table " <> value >>> table) >>> slot >>> dineIn >>> fulfillment <> (constant "takeaway at " <> value >>> time) >>> slot >>> takeaway >>> fulfillment <> (constant "delivery to " <> value >>> address) >>> slot >>> delivery >>> fulfillment <> (constant ", paid " <> value >>> paid) >>> slot >>> just >>> payment # slot # debouncer'
      containedButton { label: text # constant "Submit order " <> value >>> shortId } >>> T.do
        confirmationDialog { title: text # constant "Submit order " <> value >>> shortId >>> second <> constant "?", dismiss: text # constant "No", confirm: text # constant "Yes" } >>> lcmap (\order -> Tuple "" order) $ S.do
          body1 $
            text # constant "Provide authentication token"
          filledTextField { floatingLabel: constant "Auth token" } identity # first
        indeterminateLinearProgress # submitOrder # lcmap (\(Tuple authToken order) -> {authToken, order})
        snackbar { label: text # constant "Order " <> value >>> shortId <> constant " submitted"}
