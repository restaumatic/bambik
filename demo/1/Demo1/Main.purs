module Demo1.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Demo1.Model (PaymentMethod(..), address, authorization, card, cash, customer, delivery, dineIn, distance, firstName, forename, formal, fulfillment, high, lastName, loadOrder, low, missing, normal, order, orderId, orderSubmission, orderSubmissionFailed, paid, payment, paymentMethod, priorityAssignment, receiptPrint, remarks, shortId, summary, surname, table, takeaway, time, total)
import Effect (Effect)
import HTML as HTML
import MDC as MDC
import QualifiedDo.Alt as A
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import UI (constant, debounced, just)

main :: Effect Unit
main = HTML.body $ order "45123519" $ T.do
  loadOrder MDC.indeterminateLinearProgress
  MDC.elevation20 S.do
    MDC.caption $ HTML.staticText "Order "
    shortId $ debounced $ MDC.caption HTML.text
    MDC.card S.do
      MDC.caption $ HTML.staticText "Identifier"
      shortId $ MDC.filledTextField { floatingLabel: "Short ID" }
      orderId $ MDC.filledTextField { floatingLabel: "Unique ID" }
    customer $ MDC.card S.do
      MDC.caption $ HTML.staticText "Customer"
      MDC.caption $ HTML.staticText "Informal"
      firstName $ MDC.filledTextField { floatingLabel: "First name" }
      lastName $ MDC.filledTextField { floatingLabel: "Last name" }
      MDC.caption $ HTML.staticText "Formal"
      formal $ surname $ MDC.filledTextField { floatingLabel: "Surname" }
      formal $ forename $ MDC.filledTextField { floatingLabel: "Forename" }
    fulfillment $ MDC.card S.do
      MDC.caption $ HTML.staticText "Fulfillment"
      dineIn $ MDC.radioButton $ HTML.label $ HTML.staticText "Dine in"
      takeaway $ MDC.radioButton $ HTML.label $ HTML.staticText "Takeaway"
      delivery $ MDC.radioButton $ HTML.label $ HTML.staticText "Delivery"
      dineIn $ HTML.slot $ table $ MDC.filledTextField { floatingLabel: "Table" }
      takeaway $ HTML.slot $ time $ MDC.filledTextField { floatingLabel: "Time" }
      delivery $ HTML.slot $ address S.do
        MDC.filledTextField { floatingLabel: "Address" }
        MDC.body1 $ (S.do
          constant "Distance "
          distance
          constant " km") HTML.text
    MDC.card S.do
      MDC.caption $ HTML.staticText "Payment"
      total $ MDC.filledTextField { floatingLabel: "Total" }
      payment $ MDC.checkbox $ HTML.label $ HTML.staticText "Paid"
      payment $ just $ HTML.slot S.do
        paymentMethod S.do
          cash $ MDC.radioButton $ HTML.label $ HTML.staticText "Cash"
          card $ MDC.radioButton $ HTML.label $ HTML.staticText "Card"
        paid $ MDC.filledTextField { floatingLabel: "Paid" }
    MDC.card S.do
      MDC.caption $ HTML.staticText "Remarks"
      remarks $ MDC.filledTextArea { columns: 80, rows: 3 }
    debounced $ MDC.body1 S.do
      constant "Summary: Order " HTML.text
      shortId HTML.text
      constant " (uniquely " HTML.text
      orderId HTML.text
      constant ") for " HTML.text
      customer $ firstName HTML.text
      constant " " HTML.text
      customer $ lastName HTML.text
      constant " (formally " HTML.text
      customer $ formal $ surname HTML.text
      constant " " HTML.text
      customer $ formal $ forename HTML.text
      constant "), fulfilled as " HTML.text
      fulfillment $ dineIn $ HTML.slot S.do
        constant "dine in at table " HTML.text
        table HTML.text
      fulfillment $ takeaway $ HTML.slot S.do
        constant "takeaway at " HTML.text
        time HTML.text
      fulfillment $ delivery $ HTML.slot S.do
        constant "delivery to " HTML.text
        address S.do
          HTML.text
          constant " (" HTML.text
          distance HTML.text
          constant " km away)" HTML.text
      payment $ just $ HTML.slot S.do
        HTML.staticText ", paid "
        paid HTML.text
    T.do
      MDC.containedButton { label: Just "Submit order", icon: Just "save" }
      authorization $ MDC.simpleDialog { title: "Authorization", confirm: "Authorize" } $ T.do
        MDC.caption A.do
          HTML.staticText "Order summary: "
          summary HTML.text
        MDC.filledTextField { floatingLabel: "Auth token" }
      orderSubmission MDC.indeterminateLinearProgress
      orderSubmissionFailed $ MDC.snackbar $ HTML.staticText "Order submission failed"
      MDC.snackbar $ HTML.staticText "Order submitted"
    T.do
      MDC.containedButton { label: Just "Assign priority", icon: Just "bookmark" }
      priorityAssignment $ MDC.simpleDialog { title: "Priority assignment", confirm: "Assign" } $ T.do
        MDC.caption $ HTML.staticText "Choose one of"
        S.do
          high $ MDC.radioButton $ HTML.label $ HTML.staticText "High"
          normal $ MDC.radioButton $ HTML.label $ HTML.staticText "Normal"
          low $ MDC.radioButton $ HTML.label $ HTML.staticText "Low"
    T.do
      MDC.containedButton { label: Just "Receipt", icon: Just "file" }
      payment $ missing { method: Cash, paid: "0.00"} $ MDC.simpleDialog { title: "Missing payment", confirm: "OK" } S.do
        MDC.caption $ HTML.staticText "Choose one of"
        paymentMethod $ cash $ MDC.radioButton $ HTML.label $ HTML.staticText "Cash"
        paymentMethod $ card $ MDC.radioButton $ HTML.label $ HTML.staticText "Card"
        paid $ MDC.filledTextField { floatingLabel: "Paid" }
      receiptPrint MDC.indeterminateLinearProgress
