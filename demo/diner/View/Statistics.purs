module Diner.View.Statistics (statistics) where

import Prelude ((#), ($))

import Diner (Diner, DishSales, paidState, quantityLine, salesAmountLine, salesPage, soldAmountLine, soldQuantityLine)
import Diner.Design (amountCell, amountHeader, note, totalAmountCell, totalCell)
import Diner.View.Pager (pager)
import PUI (actedOf, PUI)
import PUI.Web (Web, inCase, text, textAt)
import PUI.Web.MDC3 (columnHeader, dataCell, dataRow, dataTable', rowHeader)
import QualifiedDo.Semigroupoid as Semigroupoid

statistics :: PUI Web Diner Diner
statistics = Semigroupoid.do
  note "Nic jeszcze nie sprzedano" # inCase @"none" paidState
  ( Semigroupoid.do
    salesTable
    pager ) # inCase @"some" paidState

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
