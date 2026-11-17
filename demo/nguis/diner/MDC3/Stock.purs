module Diner.MDC3.Stock (stock) where

import Prelude ((#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Diner (Diner, Dish, isSoldOut, portionAdded, portionRemoved)
import PUI (actedAt, fold, PUI, stepped)
import PUI.Web (Web, textAt)
import PUI.Web.MDC3 (columnHeader, dataCell, dataRow, dataTable, filledIntegerField, iconButton, iconButton')
import QualifiedDo.Semigroupoid as Semigroupoid

stock :: PUI Web Diner Diner
stock =
  dataTable "Magazyn"
    ( Semigroupoid.do
      columnHeader "Danie"
      columnHeader "Porcje"
      columnHeader "Mniej"
      columnHeader "Więcej" )
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
      VariantToRecord.do
        fold @"Ubyła porcja" portionRemoved
        fold @"Przybyła porcja" portionAdded ) # stepped
