module Diner.View (diner) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (openService)
import Diner.View.Hall (hall)
import Diner.View.Kitchen (kitchen)
import Diner.View.Pass (pass)
import Diner.View.Report (report)
import Diner.View.Statistics (statistics)
import Diner.View.Stock (stock)
import Effect (Effect)
import PUI (capture, landed, load, looped)
import PUI.Web (awaited, inCaseAt)
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
      statistics # inCaseAt @"Statystyki" @"Stanowisko" ) # looped # awaited
