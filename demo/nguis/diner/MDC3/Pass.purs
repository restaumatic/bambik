module Diner.MDC3.Pass (pass) where

import Prelude ((#), ($))

import Diner (Bill, Diner, billPlacedLine, billTotalLine, bills, billsState, numberLine, settle, tableLine)
import Diner.MDC3.Layout (note)
import PUI (foldInto, gatheredOf, PUI, stepped)
import PUI.Web (Web, inCase, provided, text)
import PUI.Web.MDC3 (button, columnHeader, dataCell, dataRow, dataTable)
import QualifiedDo.Semigroupoid as Semigroupoid

pass :: PUI Web Diner Diner
pass = Semigroupoid.do
  note "Wszystko wydane, wydawka czeka na kuchnię" # inCase @"settled" billsState
  ( Semigroupoid.do
    billTable # provided @"pending" billsState
    foldInto @"Zapłacono" settle ) # stepped

billTable :: PUI Web Diner [ "Zapłacono" :: Bill ]
billTable =
  dataTable "Wydawka"
    ( Semigroupoid.do
      columnHeader "Nr"
      columnHeader "Godzina"
      columnHeader "Stolik"
      columnHeader "Kwota"
      columnHeader "Płatność" )
    ( billRow # gatheredOf @"number" bills )

billRow :: PUI Web Bill [ "Zapłacono" :: Bill ]
billRow =
  dataRow $ Semigroupoid.do
    dataCell $ text numberLine
    dataCell $ text billPlacedLine
    dataCell $ text tableLine
    dataCell $ text billTotalLine
    dataCell $ button @"Zapłacono"
