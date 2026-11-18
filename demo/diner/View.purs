module Diner.View (diner) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (dinnerService)
import Diner.Design (station)
import Diner.View.Hall (hall)
import Diner.View.Kitchen (kitchen)
import Diner.View.Pass (pass)
import Diner.View.Report (report)
import Diner.View.Statistics (statistics)
import Diner.View.Stock (stock)
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
    ( station $ stock ) # inCaseAt @"Magazyn" @"Stanowisko"
    ( station $ hall ) # inCaseAt @"Sala" @"Stanowisko"
    ( station $ kitchen ) # inCaseAt @"Kuchnia" @"Stanowisko"
    ( station $ pass ) # inCaseAt @"Wydawka" @"Stanowisko"
    ( station $ report ) # inCaseAt @"Raport" @"Stanowisko"
    ( station $ statistics ) # inCaseAt @"Statystyki" @"Stanowisko"
  # looped # with dinnerService
