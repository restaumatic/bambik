module ReorderMDC3 (reorderMDC3) where

import Prelude (identity, (#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (acted, action, atCase, fold, looped, prefixed, static, unshown, with)
import PUI.Web (el, (:=))
import PUI.Web.MDC3 (body, buttonWith, filledTextField, group, indeterminateLinearProgress, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ReorderViewModel (openingSetlist, rotateAction, shuffleAction)

reorderMDC3 :: Effect Unit
reorderMDC3 =
  body $ Semigroupoid.do
    group @"Setlist" $ list $
      ( listItem $ prefixed (static (el "input" >>> "type" := "checkbox")) $ filledTextField @"Title" ) # acted @"id"
    RecordToVariant.do
      buttonWith @"Rotate" { icon: "sync" }
      buttonWith @"Shuffle" { icon: "shuffle" }
    VariantToRecord.do
      ( Semigroupoid.do
        indeterminateLinearProgress # action rotateAction
        unshown @"Setlist rotated" # fold identity ) # atCase @"Rotate"
      ( Semigroupoid.do
        indeterminateLinearProgress # action shuffleAction
        unshown @"Setlist shuffled" # fold identity ) # atCase @"Shuffle"
  # looped @( "Setlist" :: Array { id :: String, "Title" :: String } ) # with openingSetlist
