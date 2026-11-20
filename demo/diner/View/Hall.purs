module Diner.View.Hall (hall) where

import Prelude ((#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Diner (Diner, Dish, TableLine, addUnit, isLineSoldOut, isSinglePortion, isSoldOut, lineTotalLine, menuPage, numberLine, offerLine, orderDish, placeOrder, quantityLine, removalLine, removeDish, returnUnit, searchState, seatings, sendToKitchen, servedLine, serveTable, tableAvailability, tableFace, tableLines, tableName, tableState)
import Diner.Design (controls, floor, heading, note)
import Diner.View.Pager (pager)
import Diner.View.Search (search)
import PUI (action, foldInto, gatheredOf, PUI, stepped)
import PUI.Web (Web, attrWith, inCase, provided, text, textAt)
import PUI.Web.MDC3 (bodyMedium, bodySmall, button', columnHeader, confirmed', dataCell, dataRow, dataTable', iconButton, iconButton', indeterminateLinearProgress, tile)
import QualifiedDo.Semigroupoid as Semigroupoid

hall :: PUI Web Diner Diner
hall = Semigroupoid.do
  ( Semigroupoid.do
    floor $ ( tile @"Stolik wybrany" @"number" $ text numberLine ) # attrWith "aria-label" tableName # attrWith "style" tableFace # gatheredOf @"number" seatings
    foldInto @"Stolik wybrany" serveTable ) # stepped
  heading servedLine
  note "Stolik zajęty: zamówienie jest w kuchni lub czeka na zapłatę" # inCase @"busy" tableState
  note "Żadnych dań jeszcze nie wybrano" # inCase @"idle" tableState
  ( Semigroupoid.do
    ( RecordToVariant.do
      orderTable
      button' @"Wyślij do kuchni" { icon: "restaurant" } ) # provided @"taking" tableState
    confirmed' @"Usuń danie" { cancel: "Nie, nie usuwaj dania" } $ bodyMedium $ text removalLine
    VariantToRecord.do
      foldInto @"Zwrócono porcję" returnUnit
      foldInto @"Dodano porcję" addUnit
      foldInto @"Usuń danie" removeDish
      ( Semigroupoid.do
        indeterminateLinearProgress # action @"Wyślij do kuchni" placeOrder
        foldInto @"Zamówienie złożone" sendToKitchen ) ) # stepped
  menu # inCase @"free" tableAvailability

orderTable :: PUI Web Diner [ "Dodano porcję" :: TableLine, "Usuń danie" :: TableLine, "Zwrócono porcję" :: TableLine ]
orderTable =
  dataTable' "Zamówienie" { fullWidth: true }
    ( Semigroupoid.do
      columnHeader "Danie"
      columnHeader "Ilość" )
    ( orderRow # gatheredOf @"dish" tableLines )

orderRow :: PUI Web TableLine [ "Dodano porcję" :: TableLine, "Usuń danie" :: TableLine, "Zwrócono porcję" :: TableLine ]
orderRow =
  dataRow $ Semigroupoid.do
    dataCell $ Semigroupoid.do
      bodyMedium $ textAt @"dish"
      bodySmall $ text lineTotalLine
    dataCell $ controls $ RecordToVariant.do
      iconButton' @"Zwrócono porcję" "remove" { disabled: isSinglePortion }
      Semigroupoid.do
        text quantityLine
        iconButton' @"Dodano porcję" "add" { disabled: isLineSoldOut }
      iconButton @"Usuń danie" "delete"

menu :: PUI Web Diner Diner
menu = Semigroupoid.do
  search
  ( Semigroupoid.do
    ( Semigroupoid.do
      dataTable' "Menu" { fullWidth: true }
        ( Semigroupoid.do
          columnHeader "Danie"
          columnHeader "Zamów" )
        ( menuRow # gatheredOf @"name" menuPage )
      foldInto @"Dodaj" orderDish ) # stepped
    pager ) # inCase @"found" searchState

menuRow :: PUI Web Dish [ "Dodaj" :: Dish ]
menuRow =
  dataRow $ Semigroupoid.do
    dataCell $ Semigroupoid.do
      bodyMedium $ textAt @"name"
      bodySmall $ text offerLine
    dataCell $ button' @"Dodaj" { icon: "add", disabled: isSoldOut }
