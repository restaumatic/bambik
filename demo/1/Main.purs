module Main (main) where

import Prelude

import Data.Lens.Extra.Commons (just, missing)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Endo as Endo
import Data.Profunctor.Sum as Sum
import Model (PaymentMethod(..), address, authorization, card, cash, customer, delivery, dineIn, distance, firstName, fulfillment, high, lastName, loadOrder, low, normal, orderId, orderSubmission, orderSubmissionFailed, paid, payment, paymentMethod, priority, receiptPrint, remarks, shortId, summary, table, takeaway, time, total)
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (action, constant, debounced)
import Web (body, label, conditional, staticText, text)

main :: Effect Unit
main = body Semigroupoid.do
  action loadOrder $ MDC.indeterminateLinearProgress
  MDC.elevation20 Endo.do
    MDC.headline6 Sum.do 
      staticText "Order "
      shortId $ debounced $ text
    MDC.card Endo.do
      MDC.caption $ staticText "Identifier"
      shortId $ MDC.filledTextField { floatingLabel: "Short ID" }
      orderId $ MDC.filledTextField { floatingLabel: "Unique ID" }
    customer $ MDC.card Endo.do
      MDC.caption $ staticText "Customer"
      firstName $ MDC.filledTextField { floatingLabel: "First name" }
      lastName $ MDC.filledTextField { floatingLabel: "Last name" }
    fulfillment $ MDC.card Endo.do
      MDC.caption $ staticText "Fulfillment"
      dineIn $ MDC.radioButton $ label $ staticText "Dine in"
      takeaway $ MDC.radioButton $ label $ staticText "Takeaway"
      delivery $ MDC.radioButton $ label $ staticText "Delivery"
      dineIn $ conditional $ table $ MDC.filledTextField { floatingLabel: "Table" }
      takeaway $ conditional $ time $ MDC.filledTextField { floatingLabel: "Time" }
      delivery $ conditional $ address Endo.do
        MDC.filledTextField { floatingLabel: "Address" }
        MDC.body1 Endo.do
          constant "Distance " $ text
          distance text
          constant " km" $ text
    total $ MDC.card Endo.do
      MDC.caption $ staticText "Total"
      MDC.filledTextField { floatingLabel: "Total" }
    payment $ MDC.card Endo.do
      MDC.caption $ staticText "Payment"
      MDC.checkbox $ label $ staticText "Paid"
      just $ conditional Endo.do
        paymentMethod Endo.do
          cash $ MDC.radioButton $ label $ staticText "Cash"
          card $ MDC.radioButton $ label $ staticText "Card"
        paid $ MDC.filledTextField { floatingLabel: "Paid" }
    remarks $ MDC.card Endo.do
      MDC.caption $ staticText "Remarks"
      MDC.filledTextArea { columns: 80, rows: 3 }
    debounced $ MDC.body1 Sum.do
      constant "Summary: Order " text
      shortId text
      constant " (uniquely " text
      orderId text
      constant ") for " text
      customer $ firstName text
      constant " " text
      customer $ lastName text
      constant ", fulfilled as " text
      fulfillment $ dineIn $ conditional Sum.do
        constant "dine in at table " text
        table text
      fulfillment $ takeaway $ conditional Sum.do
        constant "takeaway at " text
        time text
      fulfillment $ delivery $ conditional Sum.do
        constant "delivery to " text
        address Sum.do
          text
          constant " (" text
          distance text
          constant " km away)" text
      payment $ just $ conditional Sum.do
        staticText ", paid "
        paid text
    Semigroupoid.do
      MDC.containedButton { label: Just "Submit order", icon: Just "save" }
      authorization $ MDC.simpleDialog { title: "Authorization", confirm: "Authorize" } Semigroupoid.do
        MDC.caption Sum.do
          staticText "Order summary: "
          summary text
        MDC.filledTextField { floatingLabel: "Auth token" }
      action orderSubmission $ MDC.indeterminateLinearProgress
      orderSubmissionFailed $ MDC.snackbar $ staticText "Order submission failed"
      MDC.snackbar $ staticText "Order submitted"
    Semigroupoid.do
      MDC.containedButton { label: Just "Assign priority", icon: Just "bookmark" }
      priority $ MDC.simpleDialog { title: "Priority", confirm: "OK" } Endo.do
        MDC.caption $ staticText "Choose one of: "
        high $ MDC.radioButton $ label $ staticText "High"
        normal $ MDC.radioButton $ label $ staticText "Normal"
        low $ MDC.radioButton $ label $ staticText "Low"
    Semigroupoid.do
      MDC.containedButton { label: Just "Receipt", icon: Just "file" }
      payment $ missing { method: Cash, paid: "0.00"} $ MDC.simpleDialog { title: "Missing payment", confirm: "OK" } Endo.do
        MDC.caption $ staticText "Choose one of: "
        paymentMethod $ cash $ MDC.radioButton $ label $ staticText "Cash"
        paymentMethod $ card $ MDC.radioButton $ label $ staticText "Card"
        paid $ MDC.filledTextField { floatingLabel: "Paid" }
      action receiptPrint $ MDC.indeterminateLinearProgress
