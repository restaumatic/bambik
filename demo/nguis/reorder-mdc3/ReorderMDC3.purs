module ReorderMDC3 (reorderMDC3) where

import Prelude (identity, (#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action, atCase, edited, fold, looped, static, with)
import PUI.Web (el, shown, (:=))
import PUI.Web.MDC3 (body, button, filledTextField, group, indeterminateLinearProgress, list, listItem, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import ReorderViewModel (openingSetlist, rotateAction, setlistRotatedLine, setlistShuffledLine, shuffleAction)

reorderMDC3 :: Effect Unit
reorderMDC3 =
  body $ ( Semigroupoid.do
    group @"Setlist" $ list $
      ( listItem $ Semigroupoid.do
        static (el "input" >>> "type" := "checkbox") # shown
        filledTextField @"Title" {} ) # edited @"id"
    RecordToVariant.do
      button @"Rotate" { icon: "sync" }
      button @"Shuffle" { icon: "shuffle" }
    VariantToVariant.do
      indeterminateLinearProgress # action rotateAction # atCase @"Rotate"
      indeterminateLinearProgress # action shuffleAction # atCase @"Shuffle"
    VariantToRecord.do
      snackbar @"Setlist rotated" setlistRotatedLine # fold identity
      snackbar @"Setlist shuffled" setlistShuffledLine # fold identity
  ) # looped @( "Setlist" :: Array { id :: String, "Title" :: String } ) # with openingSetlist
