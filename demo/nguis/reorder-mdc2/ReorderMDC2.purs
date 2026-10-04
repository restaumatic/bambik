module ReorderMDC2 (reorderMDC2) where

import Prelude (identity, (#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action, atCase, blank, edited, fold, mvu, static, toCase)
import PUI.Web (el, shown, (:=))
import PUI.Web.MDC2 (body, button, filledTextField, group, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ReorderViewModel (openingSetlist, rotateAction, shuffleAction)

reorderMDC2 :: Effect Unit
reorderMDC2 =
  body $ ( Semigroupoid.do
    group @"Setlist" $ list $
      ( listItem $ Semigroupoid.do
        static (el "input" >>> "type" := "checkbox") # shown
        filledTextField @"Title" {} ) # edited @"id"
    ( Semigroupoid.do
      RecordToVariant.do
        button @"Rotate" { icon: "sync" }
        button @"Shuffle" { icon: "shuffle" }
      VariantToVariant.do
        blank # action @{ "Setlist" :: Array { id :: String, "Title" :: String } } rotateAction # atCase @"Rotate" # toCase @"reordered" identity
        blank # action @{ "Setlist" :: Array { id :: String, "Title" :: String } } shuffleAction # atCase @"Shuffle" # toCase @"reordered" identity )
    fold @"reordered" identity
  ) # mvu @( "Setlist" :: Array { id :: String, "Title" :: String } ) openingSetlist
