module CellsBootstrap (cellsBootstrap) where

import Prelude (Unit, (#), ($), (<>), (>>>))

import CellsLogic (commit, gridRows, orderSheet, presentCells, selectCell)
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, forProperty, mvu, settled, toCase, updated)
import PUI.Web.Bootstrap (card, textField)
import PUI.Web.HTML (shown, attrWith, body, clicked, div, p, table, td, text, tr, (:=))
import QualifiedDo.Category as Category

cellsBootstrap :: Effect Unit
cellsBootstrap =
  body $
    card $ ( Category.do
        p (text @"selectedLine") # shown
        textField @"Formula (e.g. =SUM(A0:A5)*2)" {} # settled commit
        ( div >>> "style" := "overflow: auto; max-height: 420px;" $
            ( table >>> "style" := "border-collapse: collapse; font-size: 13px;" $
                ( tr $ ( clicked ( td >>> attrWith "style" cellFace $ text @"text" # forProperty ) ) # foreach @"domKey" _.cells ) # foreach @"rowKey" gridRows ) # toCase @"cellClicked" _.key ) # updated (match { cellClicked: selectCell })
    ) # settled presentCells # mvu orderSheet
cellFace :: { text :: String, kind :: [ header :: {}, cell :: {} ], status :: [ selected :: {}, unselected :: {} ] } -> String
cellFace { kind, status } = match
  { header: \_ -> "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"
  , cell: \_ -> "border: 1px solid #eee; padding: 2px 6px; min-width: 48px; height: 18px; cursor: cell;"
      <> match { selected: \_ -> " background: #cde;", unselected: \_ -> "" } status
  } kind
