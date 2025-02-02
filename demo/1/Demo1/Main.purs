module Demo1.Main (main) where

import Prelude

import Demo1.Model (address, authorization, customer, delivery, dineIn, distance, firstName, forename, formal, fulfillment, lastName, loadOrder, order, orderId, orderSubmissionFailed, paid, payment, remarks, shortId, submitOrder, summary, surname, table, takeaway, time, total)
import Effect (Effect)
import MDC (body1, caption, card, checkbox, containedButton, elevation20, filledTextArea, filledTextField, indeterminateLinearProgress, radioButton, simpleDialog, snackbar)
import QualifiedDo.Alt as A
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import Web (body, label, slot, staticText, text)
import Widget (debounced, just, static)

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
      dineIn $ radioButton { table: "1"} $ label $ staticText "Dine in"
      takeaway $ radioButton { time: "15:30"} $ label $ staticText "Takeaway"
      delivery $ radioButton { address: "Mulholland Drive 2001, Los Angeles" } $ label $ staticText "Delivery"
      dineIn $ slot $ table $ filledTextField { floatingLabel: "Table" }
      takeaway $ slot $ time $ filledTextField { floatingLabel: "Time" }
      delivery $ slot $ address S.do
        filledTextField { floatingLabel: "Address" }
        body1 $ (S.do
          static "Distance "
          distance
          static " km") text
    card S.do
      caption $ staticText "Payment"
      total $ filledTextField { floatingLabel: "Total" }
      checkbox payment { paid: "0" } $ label $ staticText "Paid"
      payment $ just $ slot $ paid $ filledTextField { floatingLabel: "Paid" }
    card S.do
      caption $ staticText "Remarks"
      remarks $ filledTextArea 80 3
    debounced $ body1 S.do
      static "Summary: Order " text
      shortId text
      static " (uniquely " text
      orderId text
      static ") for " text
      customer $ firstName text
      static " " text
      customer $ lastName text
      static " (formally " text
      customer $ formal $ surname text
      static " " text
      customer $ formal $ forename text
      static "), fulfilled as " text
      fulfillment $ dineIn $ slot S.do
        static "dine in at table " text
        table text
      fulfillment $ takeaway $ slot S.do
        static "takeaway at " text
        time text
      fulfillment $ delivery $ slot S.do
        static "delivery to " text
        address S.do
          text
          static " (" text
          distance text
          static " km away)" text
      payment $ just $ slot S.do
        staticText ", paid "
        paid text
    T.do
      containedButton $ label $ staticText "Submit order"
      authorization $ simpleDialog { title: "Authorization", confirm: "Authorize" } $ T.do
        A.do
          staticText "Order summary: "
          summary text
        filledTextField { floatingLabel: "Auth token" }
      submitOrder indeterminateLinearProgress
      orderSubmissionFailed $ snackbar $ staticText "Order submission failed"
      snackbar $ staticText "Order submitted"

