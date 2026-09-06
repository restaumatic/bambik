module ReorderMDC3 (reorderMDC3) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, blank, edited, mvu, static, updated)
import PUI.Web.HTML (el, (:=))
import PUI.Web.MDC3 (body, button, filledTextField, group, list, listItem)
import QualifiedDo.Category as Category
import ReorderLogic (openingSetlist, rotateAction, setOrder, shuffleAction)

reorderMDC3 :: Effect Unit
reorderMDC3 =
  body $ ( Category.do
      ( Category.do
          RecordToVariant.do
            button @"Rotate" { icon: "sync" }
            button @"Shuffle" { icon: "shuffle" }
          VariantToVariant.do
            blank # action rotateAction # atCase @"Rotate"
            blank # action shuffleAction # atCase @"Shuffle" ) # updated (match { reordered: setOrder })
      group @"Setlist" $ list $
        ( listItem $ RecordToRecord.do
            static (el "input" >>> "type" := "checkbox")
            filledTextField @"Title" {} ) # edited @"id"
  ) # mvu openingSetlist
