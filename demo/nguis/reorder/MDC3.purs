module Reorder.MDC3 (reorder) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (acted, action, atCase, capture, looped, static, with)
import PUI.Web (el, (:=))
import PUI.Web.MDC3 (body, button', filledTextField, group, indeterminateLinearProgress, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import Reorder (openingSetlist, rotateAction, shuffleAction)

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
      ( Semigroupoid.do
        indeterminateLinearProgress # action rotateAction
        capture @"Setlist rotated" ) # atCase @"Rotate"
      ( Semigroupoid.do
        indeterminateLinearProgress # action shuffleAction
        capture @"Setlist shuffled" ) # atCase @"Shuffle"
  # looped @( "Setlist" :: Array { id :: String, "Title" :: String } ) # with openingSetlist
