module Diner.MDC3 (diner) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (billLine, bills, billsState, cooking, dinnerService, isPlated, itemLine, kitchenState, lineTotalLine, markReady, menuCard, offerLine, orderDish, orderItemsLine, orderNumberLine, orderTableLine, paidOrders, paidState, paidTotalLine, plateLine, portionAdded, portionRemoved, quantityLine, returnUnit, sendToKitchen, settle, tableLine, tableLines, tableState, takingsLine, ticketLine, ticketState)
import Effect (Effect)
import PUI (actedAt, actedOf, fold, foldInto, gatheredOf, joined, looped, stepped, with)
import PUI.Web (inCase, inCaseAt, provided, staticText, text, textAt)
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, bodyLarge, bodyMedium, button, button', card, columnHeader, dataCell, dataRow, dataTable, filledNumberField, iconButton, list, listItem, listOf, listOfAt', option, select, tab, tabBar, titleMedium)
import QualifiedDo.Semigroupoid as Semigroupoid

diner :: Effect Unit
diner =
  body $ Semigroupoid.do
    tabBar @"Stanowisko" $ VariantToVariant.do
      tab @"Magazyn"
      tab @"Sala"
      tab @"Kuchnia"
      tab @"Wydawka"
      tab @"Raport"
    ( dataTable "Magazyn"
      ( Semigroupoid.do
        columnHeader "Danie"
        columnHeader "Porcje"
        columnHeader ""
        columnHeader "" )
      ( ( dataRow $ Semigroupoid.do
        dataCell $ textAt @"name"
        dataCell $ filledNumberField @"Porcje" @"count" @"invalid"
        ( Semigroupoid.do
          RecordToVariant.do
            dataCell $ iconButton @"Ubyła porcja" "remove"
            dataCell $ iconButton @"Przybyła porcja" "add"
          VariantToRecord.do
            fold @"Ubyła porcja" portionRemoved
            fold @"Przybyła porcja" portionAdded ) # stepped ) # actedAt @"name" @"dishes" ) ) # inCaseAt @"Magazyn" @"Stanowisko"
    ( Semigroupoid.do
      select @"Stolik" $ VariantToVariant.do
        option @"Stolik 1"
        option @"Stolik 2"
        option @"Stolik 3"
        option @"Stolik 4"
        option @"Stolik 5"
        option @"Stolik 6"
      ( Semigroupoid.do
        RecordToVariant.do
          listOf @"Zamówiono danie" @"name" menuCard $ text offerLine
          ( dataTable "Zamówienie"
            ( Semigroupoid.do
              columnHeader "Danie"
              columnHeader "Ilość"
              columnHeader "Razem"
              columnHeader "" )
            ( ( dataRow $ Semigroupoid.do
              dataCell $ textAt @"dish"
              dataCell $ text quantityLine
              dataCell $ text lineTotalLine
              dataCell $ iconButton @"Zwrócono porcję" "remove" ) # gatheredOf @"dish" tableLines ) ) # provided @"taking" tableState
          Semigroupoid.do
            bodyLarge $ text tableLine
            button' @"Wyślij do kuchni" { icon: "restaurant" } # provided @"taking" tableState
        VariantToRecord.do
          foldInto @"Zamówiono danie" orderDish
          foldInto @"Zwrócono porcję" returnUnit
          fold @"Wyślij do kuchni" sendToKitchen ) # stepped ) # inCaseAt @"Sala" @"Stanowisko"
    ( Semigroupoid.do
      ( bodyLarge $ staticText "Wszystko ugotowane, kuchnia czeka na zamówienia" ) # inCase @"cleared" kitchenState
      ( Semigroupoid.do
        div $
          ( card $ Semigroupoid.do
            titleMedium $ text ticketLine
            RecordToVariant.do
              ( listOfAt' @"Pozycja wydana" @"dish" @"lines" { selected: isPlated } $ text itemLine ) # joined @"Pozycja wydana"
              button' @"Zamówienie gotowe" { icon: "room_service" } # provided @"plated" ticketState ) # gatheredOf @"number" cooking
        VariantToRecord.do
          foldInto @"Pozycja wydana" plateLine
          foldInto @"Zamówienie gotowe" markReady ) # stepped ) # inCaseAt @"Kuchnia" @"Stanowisko"
    ( Semigroupoid.do
      ( bodyLarge $ staticText "Wszystko zapłacone" ) # inCase @"settled" billsState
      ( Semigroupoid.do
        list $
          ( listItem $ Semigroupoid.do
            text billLine
            button @"Zapłacono" ) # gatheredOf @"number" bills
        foldInto @"Zapłacono" settle ) # stepped ) # inCaseAt @"Wydawka" @"Stanowisko"
    ( Semigroupoid.do
      bodyMedium $ text takingsLine
      ( dataTable "Raport"
        ( Semigroupoid.do
          columnHeader "Nr"
          columnHeader "Stolik"
          columnHeader "Pozycje"
          columnHeader "Razem" )
        ( ( dataRow $ Semigroupoid.do
          dataCell $ text orderNumberLine
          dataCell $ text orderTableLine
          dataCell $ text orderItemsLine
          dataCell $ text paidTotalLine ) # actedOf @"number" paidOrders ) ) # inCase @"some" paidState ) # inCaseAt @"Raport" @"Stanowisko"
  # looped # with dinnerService
