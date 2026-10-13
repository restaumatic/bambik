module ReorderMDC3 (reorderMDC3) where

import Prelude (identity, (#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
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
    VariantToVariant.do
      indeterminateLinearProgress # action rotateAction # atCase @"Rotate"
      indeterminateLinearProgress # action shuffleAction # atCase @"Shuffle"
    VariantToRecord.do
      unshown @"Setlist rotated" # fold identity
      unshown @"Setlist shuffled" # fold identity
  # looped @( "Setlist" :: Array { id :: String, "Title" :: String } ) # with openingSetlist
