module Diner.View.Pager (pager) where

import Prelude ((#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Diner (Diner, isFirstPage, isLastPage, nextPage, pageLine, previousPage)
import Diner.Design (pagination)
import PUI (fold, PUI, stepped)
import PUI.Web (Web, text)
import PUI.Web.MDC3 (iconButton', labelLarge)
import QualifiedDo.Semigroupoid as Semigroupoid

pager :: PUI Web Diner Diner
pager =
  pagination $ Semigroupoid.do
    labelLarge $ text pageLine
    ( Semigroupoid.do
      RecordToVariant.do
        iconButton' @"Poprzednia strona" "chevron_left" { disabled: isFirstPage }
        iconButton' @"Następna strona" "chevron_right" { disabled: isLastPage }
      VariantToRecord.do
        fold @"Poprzednia strona" previousPage
        fold @"Następna strona" nextPage ) # stepped
