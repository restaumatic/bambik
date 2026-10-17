module Cells.MDC3 (cells) where

import Prelude (Unit, (#), ($), (>>>), const)

import Cells (columnHeaders, commit, edit, orderSheet, sheetRows)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (anyOf, anyOfAt, fold, foreach, joined2, looped, with)
import PUI.Web (providedAt, textAt, (:=))
import PUI.Web.HTML (div, inPlaceInput, table, td, tr)
import PUI.Web.MDC3 (body, doubleClickTile)
import QualifiedDo.Semigroupoid as Semigroupoid

cells :: Effect Unit
cells =
  body $ Semigroupoid.do
    ( div >>> "style" := "overflow: auto; max-height: 420px;" $
      table >>> "style" := "border-collapse: collapse; font-size: 13px;" $ Semigroupoid.do
        tr $ ( td >>> "style" := headerFace $ textAt @"text" ) # foreach @"key" (const columnHeaders)
        ( tr $ Semigroupoid.do
          td >>> "style" := headerFace $ textAt @"rowKey"
          ( td >>> "style" := cellFace $ RecordToVariant.do
            ( doubleClickTile @"Cell double-clicked" @"key" $ textAt @"text" ) # providedAt @"shown" @"mode"
            inPlaceInput @"Formula entered" @"Formula" # providedAt @"editing" @"mode" ) # anyOfAt @"key" @"cells" ) # anyOf @"rowKey" sheetRows ) # joined2 @"Cell double-clicked" @"Formula entered"
    VariantToRecord.do
      fold @"Cell double-clicked" edit
      fold @"Formula entered" commit
  # looped # with orderSheet

headerFace :: String
headerFace = "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"

cellFace :: String
cellFace = "border: 1px solid #eee; padding: 0; min-width: 72px; height: 22px; cursor: cell;"
