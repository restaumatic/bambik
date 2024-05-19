module Demo1.View
  ( order
  ) where

-- standard stuff

import Prelude

import Data.Profunctor (lcmap)
import Demo1.Model (Order, OrderId, address, authToken, customer, delivery, dineIn, distance, firstName, forename, formal, fulfillment, lastName, loadOrder, orderId, paid, payment, remarks, shortId, submitOrder, surname, table, takeaway, time, total)
import MDC (body1, caption, card, checkbox, confirmationDialog, containedButton, elevation20, filledTextArea, filledTextField, indeterminateLinearProgress, radioButton)
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import Web (Web, slot, text)
import Widget (Widget, debounced, just, static, value)


order :: Widget Web OrderId Order
order = T.do
  loadOrder indeterminateLinearProgress
  elevation20 $ S.do
    static "Order " <> shortId <<< debounced <<< value $ caption $ text
    card $ S.do
      static "Identifier" $ caption $ text
      shortId $ filledTextField { floatingLabel: "Short ID" }
      orderId $ filledTextField { floatingLabel: "Unique ID" }
    customer $ card $ S.do
      static "Customer" $ caption $ text
      static "Informal" $ caption $ text
      firstName $ filledTextField { floatingLabel: "First name" }
      lastName $ filledTextField { floatingLabel: "Last name" }
      static "Formal" $ caption $ text
      formal <<< surname $ filledTextField { floatingLabel: "Surname" }
      formal <<< forename $ filledTextField { floatingLabel: "Forename" }
    fulfillment $ card $ S.do
      static "Fulfillment" $ caption $ text
      dineIn $ radioButton { labelContent: "Dine in", default: { table: "1"} }
      takeaway $ radioButton { labelContent: "Takeaway", default: { time: "15:30"} }
      delivery $ radioButton { labelContent: "Delivery", default: { address: "Mulholland Drive 2001, Los Angeles" } }
      dineIn <<< slot <<< table $ filledTextField { floatingLabel: "Table" }
      takeaway <<< slot <<< time $ filledTextField { floatingLabel: "Time" }
      delivery <<< slot <<< address $ body1 $ S.do
        filledTextField { floatingLabel: "Address" }
        (S.do
          static "Distance "
          distance
          static " km") $ text
    card $ S.do
      static "Payment" $ caption $ text
      total $ filledTextField { floatingLabel: "Total" }
      payment $ checkbox { labelContent: "Paid", default: { paid: "0" } }
      payment <<< just <<< paid $ filledTextField { floatingLabel: "Paid" }
    card $ S.do
      static "Remarks" $ caption $ text
      remarks $ filledTextArea 80 3
    debounced <<< slot $ body1 $ (S.do
      static "Summary: Order "
      shortId <<< value
      static " (uniquely "
      orderId <<< value
      static ") for "
      customer <<< firstName <<< value
      static " "
      customer <<< lastName <<< value
      static " (formally "
      customer <<< formal <<< surname <<< value
      static " "
      customer <<< formal <<< forename <<< value
      static "), fulfilled as "
      fulfillment <<< dineIn <<< slot <<< S.do
        static "dine in at table "
        table <<< value
      fulfillment <<< takeaway <<< slot <<< S.do
        static "takeaway at "
        time <<< value
      fulfillment <<< delivery <<< slot <<< S.do
        static "delivery to "
        address <<< S.do
          value
          static " ("
          distance
          static " km away)"
      payment <<< just <<< slot <<< S.do
        static ", paid "
        paid <<< value) $ text
  T.do
    containedButton { label: static "Submit order " <> debounced <<< shortId <<< value }
    confirmationDialog { title: "Submit order", dismiss: "No", confirm: "Yes" } >>> lcmap (\submittedOrder -> { authToken: "", submittedOrder }) $ S.do
      static "Authorization required" $ body1 $ text
      authToken $ filledTextField { floatingLabel: "Auth token" }
    submitOrder indeterminateLinearProgress
    -- snackbar { label: static "Order " <> value <<< shortId <> static " submitted"}
