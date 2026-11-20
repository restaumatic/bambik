module Diner.View.Statistics (statistics) where

import Prelude ((#), ($))

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (Diner, DishSales, browseState, isFirstPage, isLastPage, nextPage, pageLine, paidState, previousPage, quantityLine, salesAmountLine, salesPage, searchState, soldAmountLine, soldQuantityLine)
import Diner.Design (amountCell, amountHeader, note, pager, station, totalAmountCell, totalCell)
import PUI (actedOf, PUI)
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
      note "Nie ma takich dań" # inCase @"none" searchState
      ( Semigroupoid.do
        salesTable
        pager pageLine { isFirst: isFirstPage, isLast: isLastPage, next: nextPage, previous: previousPage } ) # inCase @"found" searchState ) # inCase @"some" paidState

salesTable :: PUI Web Diner Diner
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
