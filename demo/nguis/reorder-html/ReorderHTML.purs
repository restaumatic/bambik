module ReorderHTML (reorderHTML) where

import Prelude (identity, (#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action, actedAt, atCase, blankStatus, fold, looped, prefixed, static, with)
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
      button @"Rotate" {}
      button @"Shuffle" {}
    VariantToVariant.do
      indeterminateLinearProgress # action rotateAction # atCase @"Rotate"
      indeterminateLinearProgress # action shuffleAction # atCase @"Shuffle"
    VariantToRecord.do
      blankStatus @"Setlist rotated" # fold identity
      blankStatus @"Setlist shuffled" # fold identity
  # looped @( "Setlist" :: Array { id :: String, "Title" :: String } ) # with openingSetlist
