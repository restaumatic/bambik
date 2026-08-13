module CellsMDC2 (cellsMDC2) where

import Prelude (Unit, identity, otherwise, (#), ($), (<>), (>>>))

import CellsLogic (commit, gridRows, orderSheet, selectCell, selectedName)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, foreach, mvu, forProperty, projected, settled, toCase, updated)
import PUI.Web.HTML (attrWith, body, clicked, div, staticText, table, td, text, tr, (:=))
import PUI.Web.MDC2 (body1, card, elevation20, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid

cellsMDC2 :: Effect Unit
cellsMDC2 =
  body $
    elevation20 $
      card { caption: "Cells" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              body1 ( RecordToRecord.do
                  staticText "Cell "
                  text # projected @"value" selectedName )
              filledTextField { floatingLabel: "Formula (e.g. =SUM(A0:A5)*2)" } # asField @"value" @"formula") # completed # settled commit
          ( div >>> "style" := "overflow: auto; max-height: 420px;" $
              ( table >>> "style" := "border-collapse: collapse; font-size: 13px;" $
                  ( tr $ ( clicked ( td >>> attrWith "style" cellFace $ text # forProperty @"value" @"text" identity ) ) # foreach @"domKey" _.cells ) # foreach @"rowKey" gridRows) # toCase @"cellClicked" _.key) # updated (match { cellClicked: selectCell })
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
