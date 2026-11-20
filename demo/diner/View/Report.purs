module Diner.View.Report (report) where

import Prelude ((#), ($))

import Diner (Diner, Receipt, orderItemsLine, paidPage, paidState, paidTotalLine, receiptLine, takingsLine)
import Diner.Design (amountCell, amountHeader, note, totalAmountCell)
import Diner.View.Pager (pager)
import PUI (actedOf, PUI)
import PUI.Web (Web, inCase, text)
import PUI.Web.MDC3 (bodyMedium, bodySmall, columnHeader, dataCell, dataRow, dataTable', rowHeader)
import QualifiedDo.Semigroupoid as Semigroupoid

report :: PUI Web Diner Diner
report = Semigroupoid.do
  note "Nic jeszcze nie sprzedano" # inCase @"none" paidState
  ( Semigroupoid.do
    paidTable
    pager ) # inCase @"some" paidState

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
      bodyMedium $ text receiptLine
      bodySmall $ text orderItemsLine
    amountCell $ text paidTotalLine
