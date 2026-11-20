module Diner.View.Pass (pass) where

import Prelude ((#), ($))

import Diner (Bill, Diner, billLine, billsPage, billsState, billTotalLine, isFirstPage, isLastPage, nextPage, paceFace, pageLine, paymentLine, previousPage, settle, waitLine)
import Diner.Design (amountCell, amountHeader, note, pager, station)
import PUI (foldInto, gatheredOf, PUI, stepped)
import PUI.Web (Web, attrWith, inCase, provided, text)
import PUI.Web.MDC3 (bodyMedium, columnHeader, confirmed', dataCell, dataRow, dataTable', iconButton)
import QualifiedDo.Semigroupoid as Semigroupoid

pass :: PUI Web Diner Diner
pass =
  station $ Semigroupoid.do
    note "Wszystko wydane, wydawka czeka na kuchnię" # inCase @"settled" billsState
    ( Semigroupoid.do
      billTable # provided @"pending" billsState
      confirmed' @"Zapłacono" { cancel: "Nie, jeszcze nie zapłacono" } $ bodyMedium $ text paymentLine
      foldInto @"Zapłacono" settle ) # stepped
    pager pageLine { isFirst: isFirstPage, isLast: isLastPage, next: nextPage, previous: previousPage } # inCase @"pending" billsState

billTable :: PUI Web Diner [ "Zapłacono" :: Bill ]
billTable =
  dataTable' "Wydawka" { fullWidth: true }
    ( Semigroupoid.do
      columnHeader "Zamówienie"
      columnHeader "Czeka"
      amountHeader "Kwota"
      columnHeader "Zapłata" )
    ( billRow # gatheredOf @"number" billsPage )

billRow :: PUI Web Bill [ "Zapłacono" :: Bill ]
billRow =
  dataRow $ Semigroupoid.do
    dataCell $ text billLine
    ( dataCell $ text waitLine ) # attrWith "style" paceFace
    amountCell $ text billTotalLine
    dataCell $ iconButton @"Zapłacono" "payments"
