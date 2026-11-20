module Diner.View.Search (search) where

import Prelude ((#), ($))

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (Diner, searchState)
import Diner.Design (note)
import PUI (PUI)
import PUI.Web (Web, inCase)
import PUI.Web.MDC3 (filledTextField', tab, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

search :: PUI Web Diner Diner
search = Semigroupoid.do
  tabBar @"Grupa" $ VariantToVariant.do
    tab @"Wszystkie"
    tab @"Przystawki"
    tab @"Zupy"
    tab @"Makarony"
    tab @"Dania główne"
    tab @"Desery"
  filledTextField' @"Szukaj" { hint: "nazwa dania" }
  note "Żadne danie nie pasuje do wyszukiwania" # inCase @"none" searchState
