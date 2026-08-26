module ReorderMDC2 (reorderMDC2) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, edited, field, mvu, pempty, updated)
import PUI.Web.HTML (body, el, (:=))
import PUI.Web.MDC2 (button, card, cardActions, elevation20, filledTextField, list, listItem)
import QualifiedDo.Category as Category
import ReorderLogic (openingSetlist, rotateAction, setOrder, shuffleAction)

reorderMDC2 :: Effect Unit
reorderMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          ( Category.do
              cardActions $ RecordToVariant.do
                button @"Rotate" { icon: "sync" }
                button @"Shuffle" { icon: "shuffle" }
              VariantToVariant.do
                pempty # action rotateAction # atCase @"Rotate"
                pempty # action shuffleAction # atCase @"Shuffle") # updated (match { reordered: setOrder })
          list
            ( ( listItem $ ( RecordToRecord.do
                  el "input" >>> "type" := "checkbox" $ pempty
                  filledTextField @"Title" {})) # edited @"id") # field @"order"
      ) # mvu openingSetlist
