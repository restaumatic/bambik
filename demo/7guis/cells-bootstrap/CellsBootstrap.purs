module CellsBootstrap (cellsBootstrap) where

import Prelude (Unit, (#), ($), (<>), (>>>))

import CellsLogic (commit, gridRows, orderSheet, selectCell, selectedLine)
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, settled, updated)
import PUI.Web.Bootstrap (body, card, textField)
import PUI.Web (attrWith, clicked, shown, text, (:=))
import PUI.Web.HTML (div, p, table, td, tr)
import QualifiedDo.Semigroupoid as Semigroupoid

cellsBootstrap :: Effect Unit
cellsBootstrap =
  body $
    card $ ( Semigroupoid.do
      p (text selectedLine) # shown
      textField @"Formula (e.g. =SUM(A0:A5)*2)" {} # settled commit
      ( div >>> "style" := "overflow: auto; max-height: 420px;" $
        ( table >>> "style" := "border-collapse: collapse; font-size: 13px;" $
          ( tr $ ( clicked @"cellClicked" _.key ( td >>> attrWith "style" cellFace $ text _.text ) ) # foreach @"domKey" _.cells ) # foreach @"rowKey" gridRows ) ) # updated (match { cellClicked: selectCell })
    ) # mvu orderSheet
cellFace :: { text :: String, kind :: [ header :: {}, cell :: {} ], status :: [ selected :: {}, unselected :: {} ] } -> String
cellFace { kind, status } = match
  { header: \_ -> "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"
  , cell: \_ -> "border: 1px solid #eee; padding: 2px 6px; min-width: 48px; height: 18px; cursor: cell;"
    <> match { selected: \_ -> " background: #cde;", unselected: \_ -> "" } status
  } kind
