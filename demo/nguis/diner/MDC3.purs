module Diner.MDC3 (diner) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (addUnit, billNumberLine, billPlacedLine, billTableLine, billTotalLine, bills, billsState, busyTables, cooking, dinnerService, dishSales, isPlated, isSoldOut, noneLeft, itemLine, kitchenState, lineTotalLine, markReady, menuCard, offerFace, offerLine, orderDish, orderItemsLine, orderNumberLine, orderTableLine, paidOrders, paidState, paidTotalLine, placeOrder, placedLine, plateLine, portionAdded, portionRemoved, quantityLine, removalLine, removeDish, returnUnit, salesAmountLine, salesQuantityLine, sendToKitchen, settle, soldQuantityLine, tableLines, tableState, takingsLine, ticketLine, ticketState)
import Effect (Effect)
import PUI (actedAt, actedOf, action, fold, foldInto, gatheredOf, joined, looped, stepped, with)
import PUI.Web ((:=), attrWith, inCase, inCaseAt, provided, staticText, text, textAt)
import PUI.Web.HTML (div, strong)
import PUI.Web.MDC3 (body, bodyLarge, bodyMedium, button, button', card, columnHeader, confirmed', dataCell, dataRow, dataTable, filledIntegerField, iconButton, iconButton', indeterminateLinearProgress, icon, listOfAt', option, select', tab, tabBar, titleMedium)
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
      tab @"Statystyki"
    ( dataTable "Magazyn"
      ( Semigroupoid.do
        columnHeader "Danie"
        columnHeader "Porcje"
        columnHeader ""
        columnHeader "" )
      ( ( dataRow $ Semigroupoid.do
        dataCell $ textAt @"name"
        dataCell $ filledIntegerField @"Porcje" @"count" @"invalid"
        ( Semigroupoid.do
          RecordToVariant.do
            dataCell $ iconButton @"Ubyła porcja" "remove"
            dataCell $ iconButton @"Przybyła porcja" "add"
          VariantToRecord.do
            fold @"Ubyła porcja" portionRemoved
            fold @"Przybyła porcja" portionAdded ) # stepped ) # actedAt @"name" @"dishes" ) ) # inCaseAt @"Magazyn" @"Stanowisko"
    ( Semigroupoid.do
      select' @"Stolik" { unavailable: busyTables } $ VariantToVariant.do
        option @"1"
        option @"2"
        option @"3"
        option @"4"
        option @"5"
        option @"6"
      ( Semigroupoid.do
        RecordToVariant.do
          div >>> "style" := "display: grid; grid-template-columns: repeat(auto-fill, minmax(160px, 1fr)); column-gap: 12px; align-self: stretch;" $
            ( card $ Semigroupoid.do
              ( titleMedium $ textAt @"name" ) # attrWith "style" offerFace
              ( bodyMedium $ text offerLine ) # attrWith "style" offerFace
              button' @"Dodaj" { icon: "add", disabled: isSoldOut } ) # gatheredOf @"name" menuCard
          ( dataTable "Zamówienie"
            ( Semigroupoid.do
              columnHeader "Danie"
              columnHeader "Ilość"
              columnHeader "Kwota"
              columnHeader ""
              columnHeader ""
              columnHeader "" )
            ( ( dataRow $ Semigroupoid.do
              dataCell $ textAt @"dish"
              dataCell $ text quantityLine
              dataCell $ text lineTotalLine
              RecordToVariant.do
                dataCell $ iconButton @"Zwrócono porcję" "remove"
                dataCell $ iconButton' @"Dodano porcję" "add" { disabled: noneLeft }
                dataCell $ iconButton @"Usuń danie" "delete" ) # gatheredOf @"dish" tableLines ) ) # provided @"taking" tableState
          Semigroupoid.do
            ( bodyLarge $ staticText "Żadnych dań jeszcze nie wybrano" ) # inCase @"idle" tableState
            button' @"Wyślij do kuchni" { icon: "restaurant" } # provided @"taking" tableState
        confirmed' @"Usuń danie" { cancel: "Nie, nie usuwaj dania" } $ bodyMedium $ text removalLine
        VariantToRecord.do
          foldInto @"Dodaj" orderDish
          foldInto @"Zwrócono porcję" returnUnit
          foldInto @"Dodano porcję" addUnit
          foldInto @"Usuń danie" removeDish
          ( Semigroupoid.do
            indeterminateLinearProgress # action @"Wyślij do kuchni" placeOrder
            foldInto @"Zamówienie złożone" sendToKitchen ) ) # stepped ) # inCaseAt @"Sala" @"Stanowisko"
    ( Semigroupoid.do
      ( bodyLarge $ staticText "Wszystko ugotowane, kuchnia czeka na zamówienia" ) # inCase @"cleared" kitchenState
      ( Semigroupoid.do
        div $
          ( card $ Semigroupoid.do
            titleMedium $ text ticketLine
            RecordToVariant.do
              ( listOfAt' @"Pozycja wydana" @"dish" @"lines" { selected: isPlated } $ Semigroupoid.do
                icon "check_circle" # inCaseAt @"plated" @"state"
                icon "radio_button_unchecked" # inCaseAt @"ordered" @"state"
                text itemLine ) # joined @"Pozycja wydana"
              button' @"Zamówienie gotowe" { icon: "room_service" } # provided @"plated" ticketState ) # gatheredOf @"number" cooking
        VariantToRecord.do
          foldInto @"Pozycja wydana" plateLine
          foldInto @"Zamówienie gotowe" markReady ) # stepped ) # inCaseAt @"Kuchnia" @"Stanowisko"
    ( Semigroupoid.do
      ( bodyLarge $ staticText "Wszystko wydane, wydawka czeka na kuchnię" ) # inCase @"settled" billsState
      ( Semigroupoid.do
        ( dataTable "Wydawka"
          ( Semigroupoid.do
            columnHeader "Nr"
            columnHeader "Godzina"
            columnHeader "Stolik"
            columnHeader "Kwota"
            columnHeader "" )
          ( ( dataRow $ Semigroupoid.do
            dataCell $ text billNumberLine
            dataCell $ text billPlacedLine
            dataCell $ text billTableLine
            dataCell $ text billTotalLine
            dataCell $ button @"Zapłacono" ) # gatheredOf @"number" bills ) ) # provided @"pending" billsState
        foldInto @"Zapłacono" settle ) # stepped ) # inCaseAt @"Wydawka" @"Stanowisko"
    ( Semigroupoid.do
      ( bodyLarge $ staticText "Nic jeszcze nie sprzedano" ) # inCase @"none" paidState
      ( dataTable "Raport"
        ( Semigroupoid.do
          columnHeader "Nr"
          columnHeader "Godzina"
          columnHeader "Stolik"
          columnHeader "Dania"
          columnHeader "Kwota" )
        ( Semigroupoid.do
          dataRow $ Semigroupoid.do
            dataCell $ strong $ staticText "Łącznie"
            dataCell $ staticText ""
            dataCell $ staticText ""
            dataCell $ staticText ""
            dataCell $ strong $ text takingsLine
          ( dataRow $ Semigroupoid.do
            dataCell $ text orderNumberLine
            dataCell $ text placedLine
            dataCell $ text orderTableLine
            dataCell $ text orderItemsLine
            dataCell $ text paidTotalLine ) # actedOf @"number" paidOrders ) ) # inCase @"some" paidState ) # inCaseAt @"Raport" @"Stanowisko"
    ( Semigroupoid.do
      ( bodyLarge $ staticText "Nic jeszcze nie sprzedano" ) # inCase @"none" paidState
      ( dataTable "Statystyki"
        ( Semigroupoid.do
          columnHeader "Danie"
          columnHeader "Ilość"
          columnHeader "Kwota" )
        ( Semigroupoid.do
          dataRow $ Semigroupoid.do
            dataCell $ strong $ staticText "Łącznie"
            dataCell $ strong $ text soldQuantityLine
            dataCell $ strong $ text takingsLine
          ( dataRow $ Semigroupoid.do
            dataCell $ textAt @"dish"
            dataCell $ text salesQuantityLine
            dataCell $ text salesAmountLine ) # actedOf @"dish" dishSales ) ) # inCase @"some" paidState ) # inCaseAt @"Statystyki" @"Stanowisko"
  # looped # with dinnerService
