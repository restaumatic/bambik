module CellsBootstrap (cellsBootstrap) where

import Prelude (Unit, otherwise, (#), ($), (<>), (>>>))

import CellsLogic (commit, gridRows, orderSheet, selectCell, selectedName)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, forProperty, projected, settled, toCase, updated)
import PUI.Web.Bootstrap (card, textField)
import PUI.Web.HTML (shown, attrWith, body, clicked, div, p, staticText, table, td, text, tr, (:=))
import QualifiedDo.Category as Category

cellsBootstrap :: Effect Unit
cellsBootstrap =
  body $
    card $ ( Category.do
        ( p $ RecordToRecord.do
            staticText "Cell "
            text @"selectedName" # projected selectedName ) # shown
        textField @"Formula (e.g. =SUM(A0:A5)*2)" {} # settled commit
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
