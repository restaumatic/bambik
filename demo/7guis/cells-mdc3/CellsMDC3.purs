module CellsMDC3 (cellsMDC3) where

import Prelude (Unit, (#), ($), (<>), (>>>))

import CellsViewModel (columnHeaders, commit, orderSheet, rowLabel, selectCell, selectedLine, sheetRows)
import Data.Variant (match)
import Effect (Effect)
import Foreign.Object (Object)
import PUI (foreach, mvu, settled, updated)
import PUI.Web (attrWith, clicked, shown, text, (:=))
import PUI.Web.HTML (div, table, td, tr)
import PUI.Web.MDC3 (body, bodyLarge, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid

cellsMDC3 :: Effect Unit
cellsMDC3 =
  body $
    ( Semigroupoid.do
      bodyLarge (text selectedLine) # shown
      filledTextField @"Formula (e.g. =SUM(A0:A5)*2)" {} # settled commit
      ( div >>> "style" := "overflow: auto; max-height: 420px;" $
        ( table >>> "style" := "border-collapse: collapse; font-size: 13px;" $ Semigroupoid.do
          ( tr $ ( td >>> "style" := headerFace $ text _.text ) # foreach @"key" @( key :: String, text :: String ) columnHeaders ) # shown
          ( tr $ Semigroupoid.do
            ( td >>> "style" := headerFace $ text rowLabel ) # shown
            ( clicked @"picked" _.key ( td >>> attrWith "style" cellFace $ text _.text ) ) # foreach @"key" _.cells ) # foreach @"rowKey" @( rowKey :: String, cells :: Array { key :: String, text :: String, status :: [ selected :: {}, unselected :: {} ] } ) sheetRows ) ) # updated (match { picked: selectCell })
    ) # mvu
      @( cells :: Object String
       , selected :: [ picked :: { name :: String }, none :: {} ]
       , "Formula (e.g. =SUM(A0:A5)*2)" :: String
       )
      orderSheet

headerFace :: String
headerFace = "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"

cellFace :: forall r1. { text :: String, status :: [ selected :: {}, unselected :: {} ] | r1 } -> String
cellFace { status } = "border: 1px solid #eee; padding: 2px 6px; min-width: 48px; height: 18px; cursor: cell;"
  <> match { selected: \_ -> " background: #cde;", unselected: \_ -> "" } status
