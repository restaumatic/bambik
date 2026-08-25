module CellsMDC3 (cellsMDC3) where

import Prelude (Unit, identity, otherwise, (#), ($), (<>), (>>>))

import CellsLogic (commit, gridRows, orderSheet, selectCell, selectedName)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, forProperty, projected, settled, toCase, updated)
import PUI.Web.HTML (shownAs, attrWith, body, clicked, div, staticText, table, td, text, tr, (:=))
import PUI.Web.MDC3 (bodyLarge, card, elevation5, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid

cellsMDC3 :: Effect Unit
cellsMDC3 =
  body $
    elevation5 $
      card $ ( Semigroupoid.do
          ( bodyLarge $ RecordToRecord.do
              staticText "Cell "
              text @"selectedName" # projected selectedName ) # shownAs identity
          filledTextField @"Formula (e.g. =SUM(A0:A5)*2)" {} # settled commit
          ( div >>> "style" := "overflow: auto; max-height: 420px;" $
              ( table >>> "style" := "border-collapse: collapse; font-size: 13px;" $
                  ( tr $ ( clicked ( td >>> attrWith "style" cellFace $ text @"text" # forProperty identity ) ) # foreach @"domKey" _.cells ) # foreach @"rowKey" gridRows) # toCase @"cellClicked" _.key) # updated (match { cellClicked: selectCell })
      ) # mvu orderSheet
cellFace :: { text :: String, header :: Boolean, sel :: Boolean } -> String
cellFace { header, sel } = cellStyle { header, sel }

cellStyle :: { header :: Boolean, sel :: Boolean } -> String
cellStyle { header, sel }
  | header = "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"
  | otherwise = "border: 1px solid #eee; padding: 2px 6px; min-width: 48px; height: 18px; cursor: cell;"
      <> (if sel then " background: #cde;" else "")
