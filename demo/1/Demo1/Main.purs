module Demo1.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Demo1.Model (PaymentMethod(..), address, authorization, cash, customer, delivery, dineIn, distance, firstName, forename, formal, fulfillment, high, lastName, loadOrder, low, missing, normal, order, orderId, orderSubmission, orderSubmissionFailed, paid, payment, paymentMethod, priorityAssignment, receiptPrint, remarks, shortId, summary, surname, table, takeaway, time, total)
import Demo1.Model as Model
import Effect (Effect)
import MDC (body1, caption, card, checkbox, containedButton, elevation20, filledTextArea, filledTextField, indeterminateLinearProgress, radioButton, simpleDialog, snackbar)
import QualifiedDo.Alt as A
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import UI (constant, debounced, just)
import Web (body, label, slot, staticText, text)

main :: Effect Unit
main = body $ order "45123519" $ T.do
  loadOrder indeterminateLinearProgress
  elevation20 S.do
    caption $ staticText "Order "
    shortId $ debounced $ caption text
    card S.do
      caption $ staticText "Identifier"
      shortId $ filledTextField { floatingLabel: "Short ID" }
      orderId $ filledTextField { floatingLabel: "Unique ID" }
    customer $ card S.do
      caption $ staticText "Customer"
      caption $ staticText "Informal"
      firstName $ filledTextField { floatingLabel: "First name" }
      lastName $ filledTextField { floatingLabel: "Last name" }
      caption $ staticText "Formal"
      formal $ surname $ filledTextField { floatingLabel: "Surname" }
      formal $ forename $ filledTextField { floatingLabel: "Forename" }
    fulfillment $ card S.do
      caption $ staticText "Fulfillment"
      dineIn $ radioButton $ label $ staticText "Dine in"
      takeaway $ radioButton $ label $ staticText "Takeaway"
      delivery $ radioButton $ label $ staticText "Delivery"
      dineIn $ slot $ table $ filledTextField { floatingLabel: "Table" }
      takeaway $ slot $ time $ filledTextField { floatingLabel: "Time" }
      delivery $ slot $ address S.do
        filledTextField { floatingLabel: "Address" }
        body1 $ (S.do
          constant "Distance "
          distance
          constant " km") text
    card S.do
      caption $ staticText "Payment"
      total $ filledTextField { floatingLabel: "Total" }
      payment $ checkbox $ label $ staticText "Paid"
      payment $ just $ slot S.do
        paymentMethod S.do
          cash $ radioButton $ label $ staticText "Cash"
          Model.card $ radioButton $ label $ staticText "Card"
        paid $ filledTextField { floatingLabel: "Paid" }
    card S.do
      caption $ staticText "Remarks"
      remarks $ filledTextArea { columns: 80, rows: 3 }
    debounced $ body1 S.do
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
      fulfillment $ dineIn $ slot S.do
        constant "dine in at table " text
        table text
      fulfillment $ takeaway $ slot S.do
        constant "takeaway at " text
        time text
      fulfillment $ delivery $ slot S.do
        constant "delivery to " text
        address S.do
          text
          constant " (" text
          distance text
          constant " km away)" text
      payment $ just $ slot S.do
        staticText ", paid "
        paid text
    T.do
      containedButton { label: Just "Submit order", icon: Just "save" }
      authorization $ simpleDialog { title: "Authorization", confirm: "Authorize" } $ T.do
        caption A.do
          staticText "Order summary: "
          summary text
        filledTextField { floatingLabel: "Auth token" }
      orderSubmission indeterminateLinearProgress
      orderSubmissionFailed $ snackbar $ staticText "Order submission failed"
      snackbar $ staticText "Order submitted"
    T.do
      containedButton { label: Just "Assign priority", icon: Just "bookmark" }
      priorityAssignment $ simpleDialog { title: "Priority assignment", confirm: "Assign" } $ T.do
        caption $ staticText "Choose one of"
        S.do
          high $ radioButton $ label $ staticText "High"
          normal $ radioButton $ label $ staticText "Normal"
          low $ radioButton $ label $ staticText "Low"
    T.do
      containedButton { label: Just "Receipt", icon: Just "file" }
      payment $ missing { method: Cash, paid: "0.00"} $ simpleDialog { title: "Missing payment", confirm: "OK" } S.do
        caption $ staticText "Choose one of"
        paymentMethod $ cash $ radioButton $ label $ staticText "Cash"
        paymentMethod $ Model.card $ radioButton $ label $ staticText "Card"
        paid $ filledTextField { floatingLabel: "Paid" }
      receiptPrint indeterminateLinearProgress
