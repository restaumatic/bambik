module Reorder.HTML (reorder) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (actedAt, action, atCase, capture, looped, static, with)
import PUI.Web (el, staticText, textAt, (:=))
import PUI.Web.HTML (body, button, dd, div, dl, dt, h3, indeterminateLinearProgress, input, label)
import QualifiedDo.Semigroupoid as Semigroupoid
import Reorder (openingSetlist, rotateAction, shuffleAction)

reorder :: Effect Unit
reorder =
  body $ div $ Semigroupoid.do
    h3 $ staticText "Setlist"
    dl $
      ( Semigroupoid.do
        dt $ Semigroupoid.do
          static (el "input" >>> "type" := "checkbox")
          textAt @"Title"
        dd $ label $ Semigroupoid.do
          staticText "Rename "
          input @"Title" "text" ) # actedAt @"id" @"Setlist"
    RecordToVariant.do
      button @"Rotate"
      button @"Shuffle"
    VariantToRecord.do
      ( Semigroupoid.do
        indeterminateLinearProgress # action rotateAction
        capture @"Setlist rotated" ) # atCase @"Rotate"
      ( Semigroupoid.do
        indeterminateLinearProgress # action shuffleAction
        capture @"Setlist shuffled" ) # atCase @"Shuffle"
  # looped @( "Setlist" :: Array { id :: String, "Title" :: String } ) # with openingSetlist
