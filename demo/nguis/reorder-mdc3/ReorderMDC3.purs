module ReorderMDC3 (reorderMDC3) where

import Prelude (identity, (#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action, atCase, blankStatus, edited, fold, looped, static, with)
import PUI.Web (el, shown, (:=))
import PUI.Web.MDC3 (body, button, filledTextField, group, list, listItem, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import ReorderViewModel (openingSetlist, rotateAction, setlistReorderedLine, shuffleAction)

reorderMDC3 :: Effect Unit
reorderMDC3 =
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
        blankStatus @"Setlist reordered" # action rotateAction # atCase @"Rotate"
        blankStatus @"Setlist reordered" # action shuffleAction # atCase @"Shuffle" )
    snackbar @"Setlist reordered" setlistReorderedLine # fold identity
  ) # looped @( "Setlist" :: Array { id :: String, "Title" :: String } ) # with openingSetlist
