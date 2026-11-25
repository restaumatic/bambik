module Diner.View.Statistics (statistics) where

import Prelude ((#), ($))

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (Diner, DishSales, Sales, browseState, isFirstSalesPage, isLastSalesPage, nextSalesPage, paidState, previousSalesPage, quantityLine, salesAmountLine, salesPage, salesPageLine, soldAmountLine, soldQuantityLine, soughtSales, soughtState)
import Diner.Design (amountCell, amountHeader, note, pager, station, totalAmountCell, totalCell)
import PUI (actedOf, derivation, derived, PUI)
import PUI.Web (Web, inCase, text, textAt)
import PUI.Web.MDC3 (columnHeader, dataCell, dataRow, dataTable', filledTextField', rowHeader, tab, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

statistics :: PUI Web Diner Diner
statistics =
  station $ Semigroupoid.do
    note "Nic jeszcze nie sprzedano" # inCase @"none" paidState
    ( Semigroupoid.do
      filledTextField' @"Szukaj w statystykach" { hint: "nazwa dania, we wszystkich grupach" }
      ( tabBar @"Grupa statystyk" $ VariantToVariant.do
        tab @"Wszystkie"
        tab @"Przystawki"
        tab @"Zupy"
        tab @"Makarony"
        tab @"Dania główne"
        tab @"Desery" ) # inCase @"browsing" browseState
      ( Semigroupoid.do
        note "Nie ma takich dań" # inCase @"none" soughtState
        ( Semigroupoid.do
          salesTable
          pager salesPageLine { isFirst: isFirstSalesPage, isLast: isLastSalesPage, next: nextSalesPage, previous: previousSalesPage } ) # inCase @"found" soughtState ) # derived (derivation @"sought" soughtSales) ) # inCase @"some" paidState

salesTable :: PUI Web Sales Sales
salesTable =
  dataTable' "Statystyki" { fullWidth: true }
    ( Semigroupoid.do
      columnHeader "Danie"
      columnHeader "Ilość"
      amountHeader "Kwota" )
    ( Semigroupoid.do
      dataRow $ Semigroupoid.do
        rowHeader "Łącznie"
        totalCell soldQuantityLine
        totalAmountCell soldAmountLine
      salesRow # actedOf @"dish" salesPage )

salesRow :: PUI Web DishSales DishSales
salesRow =
  dataRow $ Semigroupoid.do
    dataCell $ textAt @"dish"
    dataCell $ text quantityLine
    amountCell $ text salesAmountLine
