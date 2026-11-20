module Diner.View.Hall (hall) where

import Prelude (const, (#), ($), (>>>))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (Diner, Offer, Seating, Standing, TableLine, addedLine, addUnit, browseState, isAdded, isFirstPage, isLastPage, isLineSoldOut, isOffMenu, isSinglePortion, lineTotalLine, menuPage, nextPage, numberLine, offerLine, offerState, orderDish, orderTotalLine, pageLine, placeOrder, previousPage, quantityLine, removalLine, removeDish, returnUnit, searchState, seatings, sendToKitchen, servedLine, serveTable, standings, swatchFace, tableAvailability, tableFace, tableLines, tableName, tableState)
import Diner.Design (controls, entry, floor, glyph, heading, headline, legend, note, pager, scrolling, shelf, station, surfaceTile, tray)
import PUI (actedOf, action, foldInto, gatheredOf, PUI, stepped)
import PUI.Web (Web, attrWith, inCase, inCaseAt, provided, staticText, text, textAt)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (bodyMedium, bodySmall, button', columnHeader, confirmed', dataCell, dataRow, dataTable', filledTextField', iconButton, iconButton', indeterminateLinearProgress, labelLarge, labelMedium, tab, tabBar, tile)
import QualifiedDo.Semigroupoid as Semigroupoid

hall :: PUI Web Diner Diner
hall =
  station $ Semigroupoid.do
    ( Semigroupoid.do
      floor $ tableTile # gatheredOf @"number" seatings
      foldInto @"Stolik wybrany" serveTable ) # stepped
    legend $ standingEntry # actedOf @"name" (const standings)
    heading servedLine
    ( Semigroupoid.do
      tray $ Semigroupoid.do
        note "Żadnych dań jeszcze nie wybrano" # inCase @"idle" tableState
        ( Semigroupoid.do
          ( RecordToVariant.do
            orderTable
            Semigroupoid.do
              headline $ Semigroupoid.do
                labelLarge $ staticText "Łącznie"
                labelLarge $ text orderTotalLine
              button' @"Wyślij do kuchni" { icon: "restaurant" } ) # provided @"taking" tableState
          confirmed' @"Usuń danie" { cancel: "Nie, nie usuwaj dania" } $ bodyMedium $ text removalLine
          VariantToRecord.do
            foldInto @"Zwrócono porcję" returnUnit
            foldInto @"Dodano porcję" addUnit
            foldInto @"Usuń danie" removeDish
            ( Semigroupoid.do
              indeterminateLinearProgress # action @"Wyślij do kuchni" placeOrder
              foldInto @"Zamówienie złożone" sendToKitchen ) ) # stepped
      menu ) # inCase @"free" tableAvailability

tableTile :: PUI Web Seating [ "Stolik wybrany" :: Int ]
tableTile =
  ( tile @"Stolik wybrany" @"number" $ Semigroupoid.do
    text numberLine
    standingGlyph ) # attrWith "aria-label" tableName # attrWith "style" tableFace

standingEntry :: PUI Web { name :: String, state :: Standing } { name :: String, state :: Standing }
standingEntry =
  entry $ Semigroupoid.do
    span >>> attrWith "style" swatchFace $ standingGlyph
    labelMedium $ textAt @"name"

standingGlyph :: forall r. PUI Web { state :: Standing | r } { state :: Standing | r }
standingGlyph = Semigroupoid.do
  glyph "edit_note" # inCaseAt @"zamawia" @"state"
  glyph "skillet" # inCaseAt @"w kuchni" @"state"
  glyph "payments" # inCaseAt @"do zapłaty" @"state"

orderTable :: PUI Web Diner [ "Dodano porcję" :: TableLine, "Usuń danie" :: TableLine, "Zwrócono porcję" :: TableLine ]
orderTable =
  dataTable' "Zamówienie" { fullWidth: true }
    ( Semigroupoid.do
      columnHeader "Danie"
      columnHeader "Ilość" )
    ( orderRow # gatheredOf @"dish" tableLines ) # scrolling

orderRow :: PUI Web TableLine [ "Dodano porcję" :: TableLine, "Usuń danie" :: TableLine, "Zwrócono porcję" :: TableLine ]
orderRow =
  dataRow $ Semigroupoid.do
    dataCell $ Semigroupoid.do
      bodyMedium $ textAt @"dish"
      bodySmall $ text lineTotalLine
    dataCell $ controls $ RecordToVariant.do
      iconButton' @"Zwrócono porcję" "remove" { disabled: isSinglePortion }
      Semigroupoid.do
        text quantityLine
        iconButton' @"Dodano porcję" "add" { disabled: isLineSoldOut }
      iconButton @"Usuń danie" "delete"

menu :: PUI Web Diner Diner
menu = Semigroupoid.do
  filledTextField' @"Szukaj w menu" { hint: "nazwa dania, we wszystkich grupach" }
  ( tabBar @"Grupa menu" $ VariantToVariant.do
    tab @"Wszystkie"
    tab @"Przystawki"
    tab @"Zupy"
    tab @"Makarony"
    tab @"Dania główne"
    tab @"Desery" ) # inCase @"browsing" browseState
  note "Nie ma takich dań" # inCase @"none" searchState
  ( Semigroupoid.do
    ( Semigroupoid.do
      shelf $ menuTile # gatheredOf @"name" menuPage
      foldInto @"Dodaj" orderDish ) # stepped
    pager pageLine { isFirst: isFirstPage, isLast: isLastPage, next: nextPage, previous: previousPage } ) # inCase @"found" searchState

menuTile :: PUI Web Offer [ "Dodaj" :: String ]
menuTile =
  surfaceTile @"Dodaj" @"name" { disabled: isOffMenu, marked: isAdded } $ Semigroupoid.do
    headline $ Semigroupoid.do
      labelLarge $ textAt @"name"
      ( labelLarge $ text addedLine ) # inCase @"added" offerState
    labelMedium $ text offerLine
