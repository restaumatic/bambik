module CellsHTML (cellsHTML) where

import Prelude (Unit, (#), ($), (<>), (>>>))

import CellsViewModel (columnHeaders, commit, orderSheet, rowLabel, selectCell, selectedLine, sheetRows)
import Data.Variant (match)
import Effect (Effect)
import Foreign.Object (Object)
import PUI (blankStatus, fold, foreach, foreachAt, joined, looped, prefixed, settled, with)
import PUI.Pipeline as Pipeline
import PUI.Web (attrWith, clicked, staticText, text, textAt, (:=))
import PUI.Web.HTML (body, div, input, label, p, table, td, tr)

cellsHTML :: Effect Unit
cellsHTML =
  body $ div $ Pipeline.do
    p (text selectedLine)
    p ( label $ prefixed (staticText "Formula (e.g. =SUM(A0:A5)*2) ") $ input @"Formula (e.g. =SUM(A0:A5)*2)" "text" # "size" := "32" ) # settled commit
    ( div >>> "style" := "overflow: auto; max-height: 420px;" $
      ( table >>> "style" := "border-collapse: collapse; font-size: 13px;" $ Pipeline.do
        tr $ ( td >>> "style" := headerFace $ textAt @"text" ) # foreach @"key" @( key :: String, text :: String ) columnHeaders
        ( tr $ Pipeline.do
          td >>> "style" := headerFace $ text rowLabel
          ( clicked @"Cell picked" @"key" ( td >>> attrWith "style" cellFace $ textAt @"text" ) ) # foreachAt @"key" @"cells" ) # foreach @"rowKey"
            @( rowKey :: String
             , cells :: Array { key :: String, text :: String, status :: [ selected :: {}, unselected :: {} ] }
             ) sheetRows ) ) # joined @"Cell picked"
    blankStatus @"Cell picked" # fold selectCell
  # looped
    @( cells :: Object String
     , selected :: [ picked :: { name :: String }, none :: {} ]
     , "Formula (e.g. =SUM(A0:A5)*2)" :: String
     ) # with orderSheet

headerFace :: String
headerFace = "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"

cellFace :: { key :: String, text :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
cellFace { status } = "border: 1px solid #eee; padding: 2px 6px; min-width: 48px; height: 18px; cursor: cell;"
  <> match { selected: \_ -> " background: #cde;", unselected: \_ -> "" } status
