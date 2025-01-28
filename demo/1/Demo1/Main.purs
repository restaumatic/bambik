module Demo1.Main (main) where

import Prelude

import Demo1.Model (address, authToken, authorizarion, customer, delivery, dineIn, distance, firstName, forename, formal, fulfillment, lastName, left, loadOrder, order, orderId, paid, payment, remarks, right, shortId, submitOrder, surname, table, takeaway, time, total)
import Effect (Effect)
import MDC (body1, caption, card, checkbox, containedButton, elevation20, filledTextArea, filledTextField, indeterminateLinearProgress, radioButton, simpleDialog, snackbar)
import QualifiedDo.Alt as A
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import Web (body, label, text, slot)
import Widget (debounced, devoid, just, static)

main :: Effect Unit
main = body $ order "45123519" $ T.do
  loadOrder indeterminateLinearProgress
  elevation20 S.do
    static "Order " $ caption text
    shortId $ debounced $ caption text
    card S.do
      static "Identifier" $ caption text
      shortId $ filledTextField { floatingLabel: "Short ID" }
      orderId $ filledTextField { floatingLabel: "Unique ID" }
    customer $ card S.do
      static "Customer" $ caption text
      static "Informal" $ caption text
      firstName $ filledTextField { floatingLabel: "First name" }
      lastName $ filledTextField { floatingLabel: "Last name" }
      static "Formal" $ caption text
      formal $ surname $ filledTextField { floatingLabel: "Surname" }
      formal $ forename $ filledTextField { floatingLabel: "Forename" }
    fulfillment $ card S.do
      static "Fulfillment" $ caption text
      dineIn $ radioButton { table: "1"} $ static "Dine in" $ label text
      takeaway $ radioButton { time: "15:30"} $ static "Takeaway" $ label text
      delivery $ radioButton { address: "Mulholland Drive 2001, Los Angeles" } $ static "Delivery" $ label text
      dineIn $ slot $ table $ filledTextField { floatingLabel: "Table" }
      takeaway $ slot $ time $ filledTextField { floatingLabel: "Time" }
      delivery $ slot $ address S.do
        filledTextField { floatingLabel: "Address" }
        body1 $ (S.do
          static "Distance "
          distance
          static " km") text
    card S.do
      static "Payment" $ caption text
      total $ filledTextField { floatingLabel: "Total" }
      checkbox payment { paid: "0" } $ static "Paid" $ label text
      payment $ just $ slot $ paid $ filledTextField { floatingLabel: "Paid" }
    card S.do
      static "Remarks" $ caption text
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
        static ", paid " text
        paid text
    T.do
      containedButton $ label $ static "Submit order " text
      authorizarion $ simpleDialog { title: "Authorization required", confirm: "Authorize" } $ A.do
        static "Order summary: " $ text
        text
        authToken $ filledTextField { floatingLabel: "Auth token" }
      submitOrder indeterminateLinearProgress
      A.do
        right $ slot $ snackbar $ S.do
          static "Order " text
          shortId text
          static " submitted" text
        left $ slot $ snackbar $ S.do
          static "Error " text
          text
      devoid
  devoid

