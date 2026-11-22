module Diner.View.Kitchen (kitchen) where

import Prelude ((#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Diner (Diner, Line, Ticket, clockPeriod, isFirstPage, isLastPage, isPlated, itemLine, kitchenState, markReady, nextPage, paceFace, pageLine, plateDish, previousPage, readyLine, ticketLine, ticketsPage, ticketState, waitLine)
import Diner.Design (dense, heading, headline, note, pager, station)
import PUI (foldInto, gatheredOf, joined, PUI, stepped)
import PUI.Web (Web, attrWith, inCase, inCaseAt, provided, text, timestamped)
import PUI.Web.MDC3 (bodyMedium, button', card, confirmed', icon, labelLarge, layoutCell, layoutGrid, listOfAt')
import QualifiedDo.Semigroupoid as Semigroupoid

kitchen :: PUI Web Diner Diner
kitchen =
  station $ Semigroupoid.do
    note "Wszystko ugotowane, kuchnia czeka na zamówienia" # inCase @"cleared" kitchenState
    ( Semigroupoid.do
      layoutGrid $ ( layoutCell 4 $ ticket ) # gatheredOf @"number" ticketsPage
      confirmed' @"Zamówienie gotowe" { cancel: "Nie, jeszcze nie gotowe" } $ bodyMedium $ text readyLine
      VariantToRecord.do
        foldInto @"Pozycja wydana" plateDish
        foldInto @"Zamówienie gotowe" markReady ) # stepped
    pager pageLine { isFirst: isFirstPage, isLast: isLastPage, next: nextPage, previous: previousPage } # inCase @"busy" kitchenState

ticket :: PUI Web Ticket [ "Pozycja wydana" :: { event :: String, model :: Ticket }, "Zamówienie gotowe" :: Ticket ]
ticket =
  card $ Semigroupoid.do
    headline $ Semigroupoid.do
      heading ticketLine
      ( labelLarge $ text waitLine ) # attrWith "style" paceFace # timestamped @"time" clockPeriod
    RecordToVariant.do
      ( listOfAt' @"Pozycja wydana" @"dish" @"lines" { selected: isPlated } ticketItem ) # dense # joined @"Pozycja wydana"
      button' @"Zamówienie gotowe" { icon: "room_service" } # provided @"plated" ticketState

ticketItem :: PUI Web Line Line
ticketItem = Semigroupoid.do
  icon "check_circle" # inCaseAt @"plated" @"state"
  icon "radio_button_unchecked" # inCaseAt @"ordered" @"state"
  text itemLine
