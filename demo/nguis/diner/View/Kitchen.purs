module Diner.View.Kitchen (kitchen) where

import Prelude ((#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Diner (Diner, Line, Order, cooking, isPlated, itemLine, kitchenState, markReady, plateDish, ticketLine, ticketState)
import Diner.DesignSystem (note)
import PUI (foldInto, gatheredOf, joined, PUI, stepped)
import PUI.Web (Web, inCase, inCaseAt, provided, text)
import PUI.Web.MDC3 (button', card, icon, layoutCell, layoutGrid, listOfAt', titleMedium)
import QualifiedDo.Semigroupoid as Semigroupoid

kitchen :: PUI Web Diner Diner
kitchen = Semigroupoid.do
  note "Wszystko ugotowane, kuchnia czeka na zamówienia" # inCase @"cleared" kitchenState
  ( Semigroupoid.do
    layoutGrid $ ( layoutCell 6 $ ticket ) # gatheredOf @"number" cooking
    VariantToRecord.do
      foldInto @"Pozycja wydana" plateDish
      foldInto @"Zamówienie gotowe" markReady ) # stepped

ticket :: PUI Web Order [ "Pozycja wydana" :: { event :: String, model :: Order }, "Zamówienie gotowe" :: Order ]
ticket =
  card $ Semigroupoid.do
    titleMedium $ text ticketLine
    RecordToVariant.do
      ( listOfAt' @"Pozycja wydana" @"dish" @"lines" { selected: isPlated } ticketItem ) # joined @"Pozycja wydana"
      button' @"Zamówienie gotowe" { icon: "room_service" } # provided @"plated" ticketState

ticketItem :: PUI Web Line Line
ticketItem = Semigroupoid.do
  icon "check_circle" # inCaseAt @"plated" @"state"
  icon "radio_button_unchecked" # inCaseAt @"ordered" @"state"
  text itemLine
