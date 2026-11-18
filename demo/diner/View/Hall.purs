module Diner.View.Hall (hall) where

import Prelude ((#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (Diner, Dish, TableLine, addUnit, busyTables, isLineSoldOut, isSinglePortion, isSoldOut, lineTotalLine, offerLine, orderDish, placeOrder, quantityLine, removalLine, removeDish, returnUnit, sendToKitchen, tableAvailability, tableLines, tableState)
import Diner.Design (note)
import PUI (action, foldInto, gatheredAt, gatheredOf, PUI, stepped)
import PUI.Web (Web, inCase, provided, text, textAt)
import PUI.Web.MDC3 (bodyMedium, button', card, columnHeader, confirmed', dataCell, dataRow, dataTable', iconButton, iconButton', indeterminateLinearProgress, layoutCell, layoutGrid, option, select', titleMedium)
import QualifiedDo.Semigroupoid as Semigroupoid

hall :: PUI Web Diner Diner
hall = Semigroupoid.do
  tablePicker
  ( Semigroupoid.do
    RecordToVariant.do
      dishCards # provided @"free" tableAvailability
      order
    confirmed' @"Usuń danie" { cancel: "Nie, nie usuwaj dania" } $ bodyMedium $ text removalLine
    VariantToRecord.do
      foldInto @"Dodaj" orderDish
      foldInto @"Zwrócono porcję" returnUnit
      foldInto @"Dodano porcję" addUnit
      foldInto @"Usuń danie" removeDish
      ( Semigroupoid.do
        indeterminateLinearProgress # action @"Wyślij do kuchni" placeOrder
        foldInto @"Zamówienie złożone" sendToKitchen ) ) # stepped

tablePicker :: PUI Web Diner Diner
tablePicker =
  select' @"Stolik" { unavailable: busyTables } $ VariantToVariant.do
    option @"1"
    option @"2"
    option @"3"
    option @"4"
    option @"5"
    option @"6"

dishCards :: PUI Web Diner [ "Dodaj" :: Dish ]
dishCards = layoutGrid $ ( layoutCell 6 $ dishCard ) # gatheredAt @"name" @"dishes"

dishCard :: PUI Web Dish [ "Dodaj" :: Dish ]
dishCard =
  card $ Semigroupoid.do
    titleMedium $ textAt @"name"
    bodyMedium $ text offerLine
    button' @"Dodaj" { icon: "add", disabled: isSoldOut }

order :: PUI Web Diner [ "Dodano porcję" :: TableLine, "Usuń danie" :: TableLine, "Wyślij do kuchni" :: Diner, "Zwrócono porcję" :: TableLine ]
order = Semigroupoid.do
  note "Stolik zajęty: zamówienie jest w kuchni lub czeka na zapłatę" # inCase @"busy" tableState
  note "Żadnych dań jeszcze nie wybrano" # inCase @"idle" tableState
  ( RecordToVariant.do
    orderTable
    button' @"Wyślij do kuchni" { icon: "restaurant" } ) # provided @"taking" tableState

orderTable :: PUI Web Diner [ "Dodano porcję" :: TableLine, "Usuń danie" :: TableLine, "Zwrócono porcję" :: TableLine ]
orderTable =
  dataTable' "Zamówienie" { fullWidth: true }
    ( Semigroupoid.do
      columnHeader "Danie"
      columnHeader "Ilość"
      columnHeader "Kwota"
      columnHeader "Mniej"
      columnHeader "Więcej"
      columnHeader "Usuń" )
    ( orderRow # gatheredOf @"dish" tableLines )

orderRow :: PUI Web TableLine [ "Dodano porcję" :: TableLine, "Usuń danie" :: TableLine, "Zwrócono porcję" :: TableLine ]
orderRow =
  dataRow $ Semigroupoid.do
    dataCell $ textAt @"dish"
    dataCell $ text quantityLine
    dataCell $ text lineTotalLine
    RecordToVariant.do
      dataCell $ iconButton' @"Zwrócono porcję" "remove" { disabled: isSinglePortion }
      dataCell $ iconButton' @"Dodano porcję" "add" { disabled: isLineSoldOut }
      dataCell $ iconButton @"Usuń danie" "delete"
