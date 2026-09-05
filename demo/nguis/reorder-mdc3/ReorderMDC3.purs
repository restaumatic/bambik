module ReorderMDC3 (reorderMDC3) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, edited, mvu, updated, static, blank)
import PUI.Web.HTML (body, el, (:=))
import PUI.Web.MDC3 (button, elevation5, filledTextField, group, list, listItem)
import QualifiedDo.Category as Category
import ReorderLogic (openingSetlist, rotateAction, setOrder, shuffleAction)

reorderMDC3 :: Effect Unit
reorderMDC3 =
  body $
    elevation5 $ ( Category.do
        ( Category.do
            RecordToVariant.do
              button @"Rotate" { icon: "sync" }
              button @"Shuffle" { icon: "shuffle" }
            VariantToVariant.do
              blank # action rotateAction # atCase @"Rotate"
              blank # action shuffleAction # atCase @"Shuffle" ) # updated (match { reordered: setOrder })
        list
          ( ( listItem $ ( RecordToRecord.do
                static (el "input" >>> "type" := "checkbox")
                filledTextField @"Title" {} )) # edited @"id" ) # group @"Setlist"
    ) # mvu openingSetlist
