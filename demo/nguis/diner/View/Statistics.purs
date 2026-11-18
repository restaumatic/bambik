module Diner.View.Statistics (statistics) where

import Prelude ((#), ($))

import Diner (Diner, DishSales, dishSales, paidState, quantityLine, salesAmountLine, soldAmountLine, soldQuantityLine)
import Diner.DesignSystem (note, totalCell)
import PUI (actedOf, PUI)
import PUI.Web (Web, inCase, text, textAt)
import PUI.Web.MDC3 (columnHeader, dataCell, dataRow, dataTable', rowHeader)
import QualifiedDo.Semigroupoid as Semigroupoid

statistics :: PUI Web Diner Diner
statistics = Semigroupoid.do
  note "Nic jeszcze nie sprzedano" # inCase @"none" paidState
  salesTable # inCase @"some" paidState

salesTable :: PUI Web Diner Diner
salesTable =
  dataTable' "Statystyki" { fullWidth: true }
    ( Semigroupoid.do
      columnHeader "Danie"
      columnHeader "Ilość"
      columnHeader "Kwota" )
    ( Semigroupoid.do
      dataRow $ Semigroupoid.do
        rowHeader "Łącznie"
        totalCell soldQuantityLine
        totalCell soldAmountLine
      salesRow # actedOf @"dish" dishSales )

salesRow :: PUI Web DishSales DishSales
salesRow =
  dataRow $ Semigroupoid.do
    dataCell $ textAt @"dish"
    dataCell $ text quantityLine
    dataCell $ text salesAmountLine
