module Diner.MDC3.Report (report) where

import Prelude ((#), ($))

import Diner (Diner, Order, numberLine, orderItemsLine, paidOrders, paidState, paidTotalLine, placedLine, tableLine, takingsLine)
import Diner.MDC3.Layout (blankCell, note, totalCell, totalLabelCell)
import PUI (actedOf, PUI)
import PUI.Web (Web, inCase, text)
import PUI.Web.MDC3 (columnHeader, dataCell, dataRow, dataTable')
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
        totalLabelCell
        blankCell
        blankCell
        blankCell
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
