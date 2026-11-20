module Diner.View.Stock (stock) where

import Prelude ((#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (Diner, Dish, browseState, isFirstPage, isLastPage, isSoldOut, isZeroed, nextPage, pageLine, portionAdded, portionRemoved, portionsZeroed, previousPage, searchState, stockPage, zeroingLine)
import Diner.Design (controls, note, pager, pair, priceField, station)
import PUI (actedWithin, fold, PUI, stepped)
import PUI.Web (Web, inCase, text, textAt)
import PUI.Web.MDC3 (bodyMedium, columnHeader, confirmed', dataCell, dataRow, dataTable', filledIntegerField, filledTextField', iconButton, iconButton', tab, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

stock :: PUI Web Diner Diner
stock =
  station $ Semigroupoid.do
    filledTextField' @"Szukaj w magazynie" { hint: "nazwa dania, we wszystkich grupach" }
    ( tabBar @"Grupa magazynu" $ VariantToVariant.do
      tab @"Wszystkie"
      tab @"Przystawki"
      tab @"Zupy"
      tab @"Makarony"
      tab @"Dania główne"
      tab @"Desery" ) # inCase @"browsing" browseState
    note "Nie ma takich dań" # inCase @"none" searchState
    ( Semigroupoid.do
      dataTable' "Magazyn" { fullWidth: true }
        ( Semigroupoid.do
          columnHeader "Danie"
          columnHeader "Zmiana" )
        ( stockRow # actedWithin @"name" @"dishes" stockPage )
      pager pageLine { isFirst: isFirstPage, isLast: isLastPage, next: nextPage, previous: previousPage } ) # inCase @"found" searchState

stockRow :: PUI Web Dish Dish
stockRow =
  dataRow $ Semigroupoid.do
    dataCell $ Semigroupoid.do
      bodyMedium $ textAt @"name"
      pair $ Semigroupoid.do
        priceField @"Cena"
        filledIntegerField @"Porcje" @"count" @"invalid"
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
