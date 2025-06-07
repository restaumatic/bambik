module Demo1.Main (main) where

import Prelude

import Data.Lens.Extra.Commons (just, missing)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Endo as Form
import Data.Profunctor.Sum as View
import Demo1.Model (PaymentMethod(..), address, authorization, card, cash, customer, delivery, dineIn, distance, firstName, forename, formal, fulfillment, high, lastName, loadOrder, low, normal, order, orderId, orderSubmission, orderSubmissionFailed, paid, payment, paymentMethod, priority, receiptPrint, remarks, shortId, summary, surname, table, takeaway, time, total)
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Flow
import UI (action, constant, debounced)
import Web (body, label, slot, staticText, text)

main :: Effect Unit
main = body $ order "45123519" Flow.do
  action loadOrder $ MDC.indeterminateLinearProgress
  MDC.elevation20 Form.do
    MDC.caption $ staticText "Order "
    shortId $ debounced $ MDC.caption text
    MDC.card Form.do
      MDC.caption $ staticText "Identifier"
      shortId $ MDC.filledTextField { floatingLabel: "Short ID" }
      orderId $ MDC.filledTextField { floatingLabel: "Unique ID" }
    customer $ MDC.card Form.do
      MDC.caption $ staticText "Customer"
      MDC.caption $ staticText "Informal"
      firstName $ MDC.filledTextField { floatingLabel: "First name" }
      lastName $ MDC.filledTextField { floatingLabel: "Last name" }
      MDC.caption $ staticText "Formal"
      formal $ surname $ MDC.filledTextField { floatingLabel: "Surname" }
      formal $ forename $ MDC.filledTextField { floatingLabel: "Forename" }
    fulfillment $ MDC.card Form.do
      MDC.caption $ staticText "Fulfillment"
      dineIn $ MDC.radioButton $ label $ staticText "Dine in"
      takeaway $ MDC.radioButton $ label $ staticText "Takeaway"
      delivery $ MDC.radioButton $ label $ staticText "Delivery"
      dineIn $ slot $ table $ MDC.filledTextField { floatingLabel: "Table" }
      takeaway $ slot $ time $ MDC.filledTextField { floatingLabel: "Time" }
      delivery $ slot $ address Form.do
        MDC.filledTextField { floatingLabel: "Address" }
        MDC.body1 Form.do
          constant "Distance " $ text
          distance text
          constant " km" $ text
    total $ MDC.card Form.do
      MDC.caption $ staticText "Total"
      MDC.filledTextField { floatingLabel: "Total" }
    payment $ MDC.card Form.do
      MDC.caption $ staticText "Payment"
      MDC.checkbox $ label $ staticText "Paid"
      just $ slot Form.do
        paymentMethod Form.do
          cash $ MDC.radioButton $ label $ staticText "Cash"
          card $ MDC.radioButton $ label $ staticText "Card"
        paid $ MDC.filledTextField { floatingLabel: "Paid" }
    remarks $ MDC.card Form.do
      MDC.caption $ staticText "Remarks"
      MDC.filledTextArea { columns: 80, rows: 3 }
    debounced $ MDC.body1 View.do
      constant "Summary: Order " text
      shortId text
      constant " (uniquely " text
      orderId text
      constant ") for " text
      customer $ firstName text
      constant " " text
      customer $ lastName text
      constant " (formally " text
      customer $ formal $ surname text
      constant " " text
      customer $ formal $ forename text
      constant "), fulfilled as " text
      fulfillment $ dineIn $ slot View.do
        constant "dine in at table " text
        table text
      fulfillment $ takeaway $ slot View.do
        constant "takeaway at " text
        time text
      fulfillment $ delivery $ slot View.do
        constant "delivery to " text
        address View.do
          text
          constant " (" text
          distance text
          constant " km away)" text
      payment $ just $ slot View.do
        staticText ", paid "
        paid text
    Flow.do
      MDC.containedButton { label: Just "Submit order", icon: Just "save" }
      authorization $ MDC.simpleDialog { title: "Authorization", confirm: "Authorize" } Flow.do
        MDC.caption View.do
          staticText "Order summary: "
          summary text
        MDC.filledTextField { floatingLabel: "Auth token" }
      action orderSubmission $ MDC.indeterminateLinearProgress
      orderSubmissionFailed $ MDC.snackbar $ staticText "Order submission failed"
      MDC.snackbar $ staticText "Order submitted"
    Flow.do
      MDC.containedButton { label: Just "Assign priority", icon: Just "bookmark" }
      priority $ MDC.simpleDialog { title: "Priority", confirm: "OK" } Form.do
        MDC.caption $ staticText "Choose one of: "
        high $ MDC.radioButton $ label $ staticText "High"
        normal $ MDC.radioButton $ label $ staticText "Normal"
        low $ MDC.radioButton $ label $ staticText "Low"
    Flow.do
      MDC.containedButton { label: Just "Receipt", icon: Just "file" }
      payment $ missing { method: Cash, paid: "0.00"} $ MDC.simpleDialog { title: "Missing payment", confirm: "OK" } Form.do
        MDC.caption $ staticText "Choose one of: "
        paymentMethod $ cash $ MDC.radioButton $ label $ staticText "Cash"
        paymentMethod $ card $ MDC.radioButton $ label $ staticText "Card"
        paid $ MDC.filledTextField { floatingLabel: "Paid" }
      action receiptPrint $ MDC.indeterminateLinearProgress
