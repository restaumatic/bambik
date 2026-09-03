module CellsFluent (cellsFluent) where

import Prelude (Unit, (#), ($), (<>), (>>>))

import CellsLogic (commit, gridRows, orderSheet, selectCell, selectedLine)
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, settled, toCase, updated)
import PUI.Web.Fluent (body1, card, textField)
import PUI.Web.HTML (shown, attrWith, body, clicked, div, table, td, text, tr, (:=))
import QualifiedDo.Category as Category

cellsFluent :: Effect Unit
cellsFluent =
  body $
    card $ ( Category.do
        body1 (text selectedLine) # shown
        textField @"Formula (e.g. =SUM(A0:A5)*2)" {} # settled commit
        ( div >>> "style" := "overflow: auto; max-height: 420px;" $
            ( table >>> "style" := "border-collapse: collapse; font-size: 13px;" $
                ( tr $ ( clicked ( td >>> attrWith "style" cellFace $ text _.text ) ) # foreach @"domKey" _.cells ) # foreach @"rowKey" gridRows ) # toCase @"cellClicked" _.key ) # updated (match { cellClicked: selectCell })
    ) # mvu orderSheet
cellFace :: { text :: String, kind :: [ header :: {}, cell :: {} ], status :: [ selected :: {}, unselected :: {} ] } -> String
cellFace { kind, status } = match
  { header: \_ -> "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"
  , cell: \_ -> "border: 1px solid #eee; padding: 2px 6px; min-width: 48px; height: 18px; cursor: cell;"
      <> match { selected: \_ -> " background: #cde;", unselected: \_ -> "" } status
  } kind
