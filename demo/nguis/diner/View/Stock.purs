module Diner.View.Stock (stock) where

import Prelude ((#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Diner (Diner, Dish, isSoldOut, isZeroed, portionAdded, portionRemoved, portionsZeroed, zeroingLine)
import PUI (actedAt, fold, PUI, stepped)
import PUI.Web (Web, text, textAt)
import PUI.Web.MDC3 (bodyMedium, columnHeader, confirmed', dataCell, dataRow, dataTable', filledIntegerField, iconButton, iconButton')
import QualifiedDo.Semigroupoid as Semigroupoid

stock :: PUI Web Diner Diner
stock =
  dataTable' "Magazyn" { fullWidth: true }
    ( Semigroupoid.do
      columnHeader "Danie"
      columnHeader "Porcje"
      columnHeader "Mniej"
      columnHeader "Więcej"
      columnHeader "Wyzeruj" )
    ( stockRow # actedAt @"name" @"dishes" )

stockRow :: PUI Web Dish Dish
stockRow =
  dataRow $ Semigroupoid.do
    dataCell $ textAt @"name"
    dataCell $ filledIntegerField @"Porcje" @"count" @"invalid"
    ( Semigroupoid.do
      RecordToVariant.do
        dataCell $ iconButton' @"Ubyła porcja" "remove" { disabled: isSoldOut }
        dataCell $ iconButton @"Przybyła porcja" "add"
        dataCell $ iconButton' @"Wyzeruj" "restart_alt" { disabled: isZeroed }
      confirmed' @"Wyzeruj" { cancel: "Nie, nie zeruj" } $ bodyMedium $ text zeroingLine
      VariantToRecord.do
        fold @"Ubyła porcja" portionRemoved
        fold @"Przybyła porcja" portionAdded
        fold @"Wyzeruj" portionsZeroed ) # stepped
