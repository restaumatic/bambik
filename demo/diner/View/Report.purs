module Diner.View.Report (report) where

import Prelude ((#), ($))

import Diner (Diner, Receipt, clockPeriod, isFirstPage, isLastPage, nextPage, orderItemsLine, pageLine, paidPage, paidState, paidTotalLine, previousPage, receiptLine, takingsLine)
import Diner.Design (amountCell, amountHeader, note, pager, station, totalAmountCell)
import PUI (actedOf, PUI)
import PUI.Web (Web, inCase, text, timestamped)
import PUI.Web.MDC3 (bodyMedium, bodySmall, columnHeader, dataCell, dataRow, dataTable', rowHeader)
import QualifiedDo.Semigroupoid as Semigroupoid

report :: PUI Web Diner Diner
report =
  station $ Semigroupoid.do
    note "Nic jeszcze nie sprzedano" # inCase @"none" paidState
    ( Semigroupoid.do
      paidTable
      pager pageLine { isFirst: isFirstPage, isLast: isLastPage, next: nextPage, previous: previousPage } ) # inCase @"some" paidState

paidTable :: PUI Web Diner Diner
paidTable =
  dataTable' "Raport" { fullWidth: true }
    ( Semigroupoid.do
      columnHeader "Zamówienie"
      amountHeader "Kwota" )
    ( Semigroupoid.do
      dataRow $ Semigroupoid.do
        rowHeader "Łącznie"
        totalAmountCell takingsLine
      paidRow # actedOf @"number" paidPage )

paidRow :: PUI Web Receipt Receipt
paidRow =
  dataRow $ Semigroupoid.do
    dataCell $ Semigroupoid.do
      ( bodyMedium $ text receiptLine ) # timestamped @"time" clockPeriod
      bodySmall $ text orderItemsLine
    amountCell $ text paidTotalLine
