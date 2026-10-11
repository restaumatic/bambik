module ReorderMDC2 (reorderMDC2) where

import Prelude (identity, (#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action, atCase, blankStatus, edited, fold, looped, prefixed, static, with)
import PUI.Pipeline as Pipeline
import PUI.Web (el, (:=))
import PUI.Web.MDC2 (body, button, filledTextField, group, indeterminateLinearProgress, list, listItem)
import ReorderViewModel (openingSetlist, rotateAction, shuffleAction)

reorderMDC2 :: Effect Unit
reorderMDC2 =
  body $ Pipeline.do
    group @"Setlist" $ list $
      ( listItem $ prefixed (static (el "input" >>> "type" := "checkbox")) $ filledTextField @"Title" {} ) # edited @"id"
    RecordToVariant.do
      button @"Rotate" { icon: "sync" }
      button @"Shuffle" { icon: "shuffle" }
    VariantToVariant.do
      indeterminateLinearProgress # action rotateAction # atCase @"Rotate"
      indeterminateLinearProgress # action shuffleAction # atCase @"Shuffle"
    VariantToRecord.do
      blankStatus @"Setlist rotated" # fold identity
      blankStatus @"Setlist shuffled" # fold identity
  # looped @( "Setlist" :: Array { id :: String, "Title" :: String } ) # with openingSetlist
