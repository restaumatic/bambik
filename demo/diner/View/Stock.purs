module Diner.View.Stock (stock) where

import Prelude ((#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Diner (Diner, Dish, isSoldOut, isZeroed, portionAdded, portionRemoved, portionsZeroed, searchState, stockPage, zeroingLine)
import Diner.Design (controls)
import Diner.View.Pager (pager)
import Diner.View.Search (search)
import PUI (actedWithin, fold, PUI, stepped)
import PUI.Web (Web, inCase, text, textAt)
import PUI.Web.MDC3 (bodyMedium, columnHeader, confirmed', dataCell, dataRow, dataTable', filledIntegerField, iconButton, iconButton')
import QualifiedDo.Semigroupoid as Semigroupoid

stock :: PUI Web Diner Diner
stock = Semigroupoid.do
  search
  ( Semigroupoid.do
    dataTable' "Magazyn" { fullWidth: true }
      ( Semigroupoid.do
        columnHeader "Danie"
        columnHeader "Porcje"
        columnHeader "Zmiana" )
      ( stockRow # actedWithin @"name" @"dishes" stockPage )
    pager ) # inCase @"found" searchState

stockRow :: PUI Web Dish Dish
stockRow =
  dataRow $ Semigroupoid.do
    dataCell $ textAt @"name"
    dataCell $ filledIntegerField @"Porcje" @"count" @"invalid"
    dataCell $
      ( Semigroupoid.do
        controls $ RecordToVariant.do
          iconButton' @"Ubyła porcja" "remove" { disabled: isSoldOut }
          iconButton @"Przybyła porcja" "add"
          iconButton' @"Wyzeruj" "restart_alt" { disabled: isZeroed }
        confirmed' @"Wyzeruj" { cancel: "Nie, nie zeruj" } $ bodyMedium $ text zeroingLine
        VariantToRecord.do
          fold @"Ubyła porcja" portionRemoved
          fold @"Przybyła porcja" portionAdded
          fold @"Wyzeruj" portionsZeroed ) # stepped
