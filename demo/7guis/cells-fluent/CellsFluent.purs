module CellsFluent (cellsFluent) where

import Prelude (Unit, identity, otherwise, (#), ($), (<>), (>>>))

import CellsLogic (commit, gridRows, orderSheet, selectCell, selectedName)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, foreach, mvu, forProperty, projected, settled, toCase, updated)
import PUI.Web.Fluent (body1, card, textField)
import PUI.Web.HTML (attrWith, body, clicked, div, staticText, table, td, text, tr, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid

cellsFluent :: Effect Unit
cellsFluent =
  body $
    card { caption: "Cells" } $ ( Semigroupoid.do
        ( RecordToRecord.do
            body1 ( RecordToRecord.do
                staticText "Cell "
                text @"selectedName" # projected selectedName )
            textField @"formula" { label: "Formula (e.g. =SUM(A0:A5)*2)" }) # completed # settled commit
        ( div >>> "style" := "overflow: auto; max-height: 420px;" $
            ( table >>> "style" := "border-collapse: collapse; font-size: 13px;" $
                ( tr $ ( clicked ( td >>> attrWith "style" cellFace $ text @"text" # forProperty identity ) ) # foreach @"domKey" _.cells ) # foreach @"rowKey" gridRows) # toCase @"cellClicked" _.key) # updated (match { cellClicked: selectCell })
    ) # mvu orderSheet

-- closed signature states the clicked content's row (the row-stating
-- exception): the leaf reads text, the style reads header/sel
cellFace :: { text :: String, header :: Boolean, sel :: Boolean } -> String
cellFace { header, sel } = cellStyle { header, sel }

cellStyle :: { header :: Boolean, sel :: Boolean } -> String
cellStyle { header, sel }
  | header = "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"
  | otherwise = "border: 1px solid #eee; padding: 2px 6px; min-width: 48px; height: 18px; cursor: cell;"
      <> (if sel then " background: #cde;" else "")
