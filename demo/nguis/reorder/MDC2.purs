module Reorder.MDC2 (reorder) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (acted, action, capture, fold, looped, static, with)
import PUI.Web (el, (:=))
import PUI.Web.MDC2 (body, button', filledTextField, group, indeterminateLinearProgress, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import Reorder (openingSetlist, rotateSetlist, shuffleAction)

reorder :: Effect Unit
reorder =
  body $ Semigroupoid.do
    group @"Setlist" $ list $
      ( listItem $ Semigroupoid.do
        static (el "input" >>> "type" := "checkbox")
        filledTextField @"Title" ) # acted @"id"
    RecordToVariant.do
      button' @"Rotate" { icon: "sync" }
      button' @"Shuffle" { icon: "shuffle" }
    VariantToRecord.do
      fold @"Rotate" rotateSetlist
      ( Semigroupoid.do
        indeterminateLinearProgress # action @"Shuffle" @( "Setlist shuffled" :: _ ) shuffleAction
        capture @"Setlist shuffled" )
  # looped @( "Setlist" :: Array { id :: String, "Title" :: String } ) # with openingSetlist
