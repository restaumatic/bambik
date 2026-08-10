module ReorderMDC2 (reorderMDC2) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, asCase, asField, edited, field, mvu, atCase, pempty, silence, updated)
import PUI.Web.HTML (body, el, (:=))
import PUI.Web.MDC2 (button, card, cardActions, elevation20, filledTextField, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ReorderLogic (openingSetlist, rotateAction, setOrder, shuffleAction)

reorderMDC2 :: Effect Unit
reorderMDC2 =
  body $
    elevation20 $
      card { caption: "Reorder" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              cardActions $ RecordToVariant.do
                button { label: "Rotate", icon: "sync" } # asCase @"clicked" @"rotate"
                button { label: "Shuffle", icon: "shuffle" } # asCase @"clicked" @"shuffle"
              VariantToVariant.do
                silence # action rotateAction # atCase @"rotate"
                silence # action shuffleAction # atCase @"shuffle") # updated (match { reordered: setOrder })
          list
            ( ( listItem $ ( RecordToRecord.do
                  el "input" >>> "type" := "checkbox" $ pempty
                  filledTextField { floatingLabel: "Title" } # asField @"value" @"title")) # edited @"id") # field @"order"
      ) # mvu openingSetlist
