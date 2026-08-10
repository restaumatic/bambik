module CellsBootstrap (cellsBootstrap) where

import Prelude (Unit, identity, otherwise, (#), ($), (<>), (>>>))

import CellsLogic (commit, gridRows, orderSheet, selectCell, selectedName)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, foreach, mvu, forProperty, projected, settled, toCase, updated)
import PUI.Web.Bootstrap (card, textField)
import PUI.Web.HTML (attrWith, body, clicked, div, p, staticText, table, td, text, tr, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid

cellsBootstrap :: Effect Unit
cellsBootstrap =
  body $
    card { caption: "Cells" } $ ( Semigroupoid.do
        ( RecordToRecord.do
            p ( RecordToRecord.do
                staticText "Cell "
                text # projected @"value" selectedName )
            textField { label: "Formula (e.g. =SUM(A0:A5)*2)" } # asField @"value" @"formula") # completed # settled commit
        ( div >>> "style" := "overflow: auto; max-height: 420px;" $
            ( table >>> "style" := "border-collapse: collapse; font-size: 13px;" $
                ( tr $ ( clicked ( td >>> attrWith "style" (\c -> cellStyle { header: c.header, sel: c.sel }) $ text # forProperty @"value" @"text" identity ) ) # foreach @"domKey" _.cells ) # foreach @"rowKey" gridRows) # toCase @"cellClicked" _.key) # updated (match { cellClicked: selectCell })
    ) # mvu orderSheet

cellStyle :: { header :: Boolean, sel :: Boolean } -> String
cellStyle { header, sel }
  | header = "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"
  | otherwise = "border: 1px solid #eee; padding: 2px 6px; min-width: 48px; height: 18px; cursor: cell;"
      <> (if sel then " background: #cde;" else "")
