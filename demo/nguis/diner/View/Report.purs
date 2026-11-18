module Diner.View.Report (report) where

import Prelude (identity, (#), ($))

import Diner (Diner, Order, numberLine, orderItemsLine, paidOrders, paidState, paidTotalLine, placedLine, tableLine, takingsLine)
import Diner.Design (note, totalCell)
import PUI (actedOf, PUI)
import PUI.Web (Web, inCase, text)
import PUI.Web.MDC3 (columnHeader, dataCell, dataRow, dataTable', rowHeader)
import QualifiedDo.Semigroupoid as Semigroupoid

report :: PUI Web Diner Diner
report = Semigroupoid.do
  note "Nic jeszcze nie sprzedano" # inCase @"none" paidState
  paidTable # inCase @"some" paidState

paidTable :: PUI Web Diner Diner
paidTable =
  dataTable' "Raport" { fullWidth: true }
    ( Semigroupoid.do
      columnHeader "Nr"
      columnHeader "Godzina"
      columnHeader "Stolik"
      columnHeader "Dania"
      columnHeader "Kwota" )
    ( Semigroupoid.do
      dataRow $ Semigroupoid.do
        rowHeader "Łącznie"
        dataCell identity
        dataCell identity
        dataCell identity
        totalCell takingsLine
      paidRow # actedOf @"number" paidOrders )

paidRow :: PUI Web Order Order
paidRow =
  dataRow $ Semigroupoid.do
    dataCell $ text numberLine
    dataCell $ text placedLine
    dataCell $ text tableLine
    dataCell $ text orderItemsLine
    dataCell $ text paidTotalLine
