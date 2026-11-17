module Diner.MDC3 (diner) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (dinnerService)
import Diner.MDC3.Hall (hall)
import Diner.MDC3.Kitchen (kitchen)
import Diner.MDC3.Pass (pass)
import Diner.MDC3.Report (report)
import Diner.MDC3.Statistics (statistics)
import Diner.MDC3.Stock (stock)
import Effect (Effect)
import PUI (looped, with)
import PUI.Web (inCaseAt)
import PUI.Web.MDC3 (body, tab, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

diner :: Effect Unit
diner =
  body $ Semigroupoid.do
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
  # looped # with dinnerService
