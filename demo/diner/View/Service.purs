module Diner.View.Service (service) where

import Prelude ((#), ($))

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (Diner, clockPeriod, secondPassed)
import Diner.Design (station)
import Diner.View.Hall (hall)
import Diner.View.Kitchen (kitchen)
import Diner.View.Pass (pass)
import Diner.View.Report (report)
import Diner.View.Statistics (statistics)
import Diner.View.Stock (stock)
import PUI (fold, PUI, stepped, ticks)
import PUI.Web (Web, inCaseAt)
import PUI.Web.MDC3 (tab, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

service :: PUI Web Diner Diner
service = Semigroupoid.do
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
  ( Semigroupoid.do
    ticks @"Minęła sekunda" clockPeriod
    fold @"Minęła sekunda" secondPassed ) # stepped
