module Diner.View (diner) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (clockPeriod, openService, secondPassed)
import Diner.View.Hall (hall)
import Diner.View.Kitchen (kitchen)
import Diner.View.Pass (pass)
import Diner.View.Report (report)
import Diner.View.Statistics (statistics)
import Diner.View.Stock (stock)
import Effect (Effect)
import PUI (capture, fold, landed, load, looped, stepped, ticks)
import PUI.Web (inCaseAt)
import PUI.Web.MDC3 (body, indeterminateLinearProgress, tab, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

diner :: Effect Unit
diner =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      indeterminateLinearProgress # load openService
      capture @"Serwis otwarty" ) # landed
    ( Semigroupoid.do
      tabBar @"Stanowisko" $ VariantToVariant.do
        tab @"Magazyn"
        tab @"Sala"
        tab @"Kuchnia"
        tab @"Wydawka"
        tab @"Raport"
        tab @"Statystyki"
      stock # inCaseAt @"Magazyn" @"Stanowisko"
      hall # inCaseAt @"Sala" @"Stanowisko"
      kitchen # inCaseAt @"Kuchnia" @"Stanowisko"
      pass # inCaseAt @"Wydawka" @"Stanowisko"
      report # inCaseAt @"Raport" @"Stanowisko"
      statistics # inCaseAt @"Statystyki" @"Stanowisko"
      ( Semigroupoid.do
        ticks @"Minęła sekunda" clockPeriod
        fold @"Minęła sekunda" secondPassed ) # stepped ) # looped
