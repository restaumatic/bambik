module ReorderHTML (reorderHTML) where

import Prelude (identity, (#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, actedAt, atCase, unshown, fold, looped, prefixed, static, with)
import PUI.Web (el, staticText, textAt, (:=))
import PUI.Web.HTML (body, button, dd, div, dl, dt, h3, indeterminateLinearProgress, input, label)
import QualifiedDo.Semigroupoid as Semigroupoid
import ReorderViewModel (openingSetlist, rotateAction, shuffleAction)

reorderHTML :: Effect Unit
reorderHTML =
  body $ div $ Semigroupoid.do
    prefixed (h3 $ staticText "Setlist") $ dl $
      ( prefixed ( dt $ prefixed (static (el "input" >>> "type" := "checkbox")) $ textAt @"Title" ) $
        dd $ label $ prefixed (staticText "Rename ") $ input @"Title" "text" ) # actedAt @"id" @"Setlist"
    RecordToVariant.do
      button @"Rotate"
      button @"Shuffle"
    VariantToRecord.do
      ( Semigroupoid.do
        indeterminateLinearProgress # action rotateAction
        unshown @"Setlist rotated" # fold identity ) # atCase @"Rotate"
      ( Semigroupoid.do
        indeterminateLinearProgress # action shuffleAction
        unshown @"Setlist shuffled" # fold identity ) # atCase @"Shuffle"
  # looped @( "Setlist" :: Array { id :: String, "Title" :: String } ) # with openingSetlist
