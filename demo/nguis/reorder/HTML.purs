module Reorder.HTML (reorder) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (actedAt, action, capture, fold, looped, static, with)
import PUI.Web (el, staticText, textAt, (:=))
import PUI.Web.HTML (body, button, dd, div, dl, dt, h3, indeterminateLinearProgress, input, label)
import QualifiedDo.Semigroupoid as Semigroupoid
import Reorder (openingSetlist, rotateSetlist, shuffleAction)

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
      fold @"Rotate" rotateSetlist
      ( Semigroupoid.do
        indeterminateLinearProgress # action @"Shuffle" @( "Setlist shuffled" :: _ ) shuffleAction
        capture @"Setlist shuffled" )
  # looped @( "Setlist" :: Array { id :: String, "Title" :: String } ) # with openingSetlist
