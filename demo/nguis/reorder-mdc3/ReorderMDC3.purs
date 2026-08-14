module ReorderMDC3 (reorderMDC3) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, edited, field, mvu, pempty, updated)
import PUI.Web.HTML (body, el, (:=))
import PUI.Web.MDC3 (button, card, cardActions, elevation5, filledTextField, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ReorderLogic (openingSetlist, rotateAction, setOrder, shuffleAction)

reorderMDC3 :: Effect Unit
reorderMDC3 =
  body $
    elevation5 $
      card { caption: "Reorder" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              cardActions $ RecordToVariant.do
                button @"rotate" { label: "Rotate", icon: "sync" }
                button @"shuffle" { label: "Shuffle", icon: "shuffle" }
              VariantToVariant.do
                pempty # action rotateAction # atCase @"rotate"
                pempty # action shuffleAction # atCase @"shuffle") # updated (match { reordered: setOrder })
          list
            ( ( listItem $ ( RecordToRecord.do
                  el "input" >>> "type" := "checkbox" $ pempty
                  filledTextField @"title" {})) # edited @"id") # field @"order"
      ) # mvu openingSetlist
