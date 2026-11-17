module Diner.MDC3.Stock (stock) where

import Prelude ((#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Diner (Diner, Dish, isSoldOut, isStockEmpty, portionAdded, portionRemoved, stockZeroed)
import PUI (actedAt, fold, PUI, stepped)
import PUI.Web (Web, staticText, textAt)
import PUI.Web.MDC3 (bodyMedium, button', columnHeader, confirmed', dataCell, dataRow, dataTable', filledIntegerField, iconButton, iconButton')
import QualifiedDo.Semigroupoid as Semigroupoid

stock :: PUI Web Diner Diner
stock = Semigroupoid.do
  stockTable
  ( Semigroupoid.do
    button' @"Wyzeruj" { icon: "restart_alt", disabled: isStockEmpty }
    confirmed' @"Wyzeruj" { cancel: "Nie, nie zeruj" } $ bodyMedium $ staticText "Wyzerować porcje wszystkich dań?"
    fold @"Wyzeruj" stockZeroed ) # stepped

stockTable :: PUI Web Diner Diner
stockTable =
  dataTable' "Magazyn" { fullWidth: true }
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
