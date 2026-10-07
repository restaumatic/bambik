module ReorderMDC2 (reorderMDC2) where

import Prelude (identity, (#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action, atCase, blank, edited, fold, looped, static, toCase, with)
import PUI.Web (el, shown, (:=))
import PUI.Web.MDC2 (body, button, filledTextField, group, list, listItem, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import ReorderViewModel (openingSetlist, rotateAction, setlistReorderedLine, shuffleAction)

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
        blank # action @{ "Setlist" :: Array { id :: String, "Title" :: String } } rotateAction # atCase @"Rotate" # toCase @"Setlist reordered" identity
        blank # action @{ "Setlist" :: Array { id :: String, "Title" :: String } } shuffleAction # atCase @"Shuffle" # toCase @"Setlist reordered" identity )
    snackbar @"Setlist reordered" setlistReorderedLine # fold identity
  ) # looped @( "Setlist" :: Array { id :: String, "Title" :: String } ) # with openingSetlist
