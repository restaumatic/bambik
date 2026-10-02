module ReorderMDC3 (reorderMDC3) where

import Prelude (identity, (#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, blank, edited, mvu, static, toCase, updated)
import PUI.Web (el, shown, (:=))
import PUI.Web.MDC3 (body, button, filledTextField, group, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ReorderViewModel (openingSetlist, rotateAction, setOrder, shuffleAction)

reorderMDC3 :: Effect Unit
reorderMDC3 =
  body $ ( Semigroupoid.do
    ( Semigroupoid.do
      RecordToVariant.do
        button @"Rotate" { icon: "sync" }
        button @"Shuffle" { icon: "shuffle" }
      VariantToVariant.do
        blank # action @((Array { id :: String, "Title" :: String })) rotateAction # atCase @"Rotate" # toCase @"reordered" identity
        blank # action @((Array { id :: String, "Title" :: String })) shuffleAction # atCase @"Shuffle" # toCase @"reordered" identity ) # updated (match { reordered: setOrder })
    group @"Setlist" $ list $
      ( listItem $ Semigroupoid.do
        static (el "input" >>> "type" := "checkbox") # shown
        filledTextField @"Title" {} ) # edited @"id"
  ) # mvu @( "Setlist" :: Array { id :: String, "Title" :: String } ) openingSetlist
