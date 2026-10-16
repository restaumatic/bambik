module Cells.MDC3 (cells) where

import Prelude (Unit, (#), ($), (<>), (>>>), const)

import Cells (columnHeaders, commit, orderSheet, selectCell, selectedLine, sheetRows)
import Data.Variant (match)
import Effect (Effect)
import PUI (anyOf, anyOfAt, fold, foreach, joined, looped, settled, with)
import PUI.Web (attrWith, text, textAt, (:=))
import PUI.Web.HTML (div, table, td, tr)
import PUI.Web.MDC3 (body, bodyLarge, filledTextField', tile)
import QualifiedDo.Semigroupoid as Semigroupoid

cells :: Effect Unit
cells =
  body $ Semigroupoid.do
    bodyLarge $ text selectedLine
    filledTextField' @"Formula" { hint: "e.g. =SUM(A0:A5)*2" } # settled commit
    ( div >>> "style" := "overflow: auto; max-height: 420px;" $
      table >>> "style" := "border-collapse: collapse; font-size: 13px;" $ Semigroupoid.do
        tr $ ( td >>> "style" := headerFace $ textAt @"text" ) # foreach @"key" (const columnHeaders)
        ( tr $ Semigroupoid.do
          td >>> "style" := headerFace $ textAt @"rowKey"
          ( td >>> "style" := "padding: 0;" $ attrWith "style" cellFace $ tile @"Cell picked" @"key" $ textAt @"text" ) # anyOfAt @"key" @"cells" ) # anyOf @"rowKey" sheetRows ) # joined @"Cell picked"
    fold @"Cell picked" selectCell
  # looped # with orderSheet

headerFace :: String
headerFace = "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"

cellFace :: { key :: String, text :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
cellFace { status } = "border: 1px solid #eee; padding: 2px 6px; min-width: 48px; height: 18px; cursor: cell;"
  <> match { selected: \_ -> " background: #cde;", unselected: \_ -> "" } status
