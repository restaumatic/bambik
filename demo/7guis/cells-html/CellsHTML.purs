module CellsHTML (cellsHTML) where

import Prelude (Unit, otherwise, (#), ($), (<>), (>>>))

import CellsLogic (commit, gridRows, orderSheet, selectCell, selectedName)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (field, foreach, mvu, forProperty, projected, settled, toCase, updated)
import PUI.Web.HTML (shownAlways, attrWith, body, clicked, div, input, label, p, staticText, table, td, text, tr, (:=))
import QualifiedDo.Semigroupoid as Pipeline

cellsHTML :: Effect Unit
cellsHTML =
  body $ div $ ( Pipeline.do
      ( p $ RecordToRecord.do
          staticText "Cell "
          text @"selectedName" # projected selectedName ) # shownAlways
      p ( label $ Pipeline.do
          (staticText "Formula (e.g. =SUM(A0:A5)*2) ") # shownAlways
          "size" := "32" $ input "text" # field @"Formula (e.g. =SUM(A0:A5)*2)" ) # settled commit
      ( div >>> "style" := "overflow: auto; max-height: 420px;" $
          ( table >>> "style" := "border-collapse: collapse; font-size: 13px;" $
              ( tr $ ( clicked ( td >>> attrWith "style" cellFace $ text @"text" # forProperty ) ) # foreach @"domKey" _.cells ) # foreach @"rowKey" gridRows) # toCase @"cellClicked" _.key) # updated (match { cellClicked: selectCell })
  ) # mvu orderSheet
cellFace :: { text :: String, header :: Boolean, sel :: Boolean } -> String
cellFace { header, sel } = cellStyle { header, sel }

cellStyle :: { header :: Boolean, sel :: Boolean } -> String
cellStyle { header, sel }
  | header = "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"
  | otherwise = "border: 1px solid #eee; padding: 2px 6px; min-width: 48px; height: 18px; cursor: cell;"
      <> (if sel then " background: #cde;" else "")
