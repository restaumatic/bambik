module CellsHTML (cellsHTML) where

import Prelude (Unit, (#), ($), (<>), (>>>))

import CellsViewModel (columnHeaders, commit, orderSheet, rowLabel, selectCell, selectedLine, sheetRows)
import Data.Variant (match)
import Effect (Effect)
import Foreign.Object (Object)
import PUI (fold, foreach, joined, mvu, settled)
import PUI.Web (attrWith, clicked, shown, staticText, text, (:=))
import PUI.Web.HTML (body, div, input, label, p, table, td, tr)
import QualifiedDo.Semigroupoid as Semigroupoid

cellsHTML :: Effect Unit
cellsHTML =
  body $ div $ ( Semigroupoid.do
    p (text selectedLine) # shown
    p ( label $ Semigroupoid.do
      (staticText @"Formula (e.g. =SUM(A0:A5)*2) ") # shown
      input @"Formula (e.g. =SUM(A0:A5)*2)" "text" # "size" := "32" ) # settled commit
    ( div >>> "style" := "overflow: auto; max-height: 420px;" $
      ( table >>> "style" := "border-collapse: collapse; font-size: 13px;" $ Semigroupoid.do
        ( tr $ ( td >>> "style" := headerFace $ text _.text ) # foreach @"key" @( key :: String, text :: String ) columnHeaders ) # shown
        ( tr $ Semigroupoid.do
          ( td >>> "style" := headerFace $ text rowLabel ) # shown
          ( clicked @"picked" _.key ( td >>> attrWith "style" cellFace $ text _.text ) ) # foreach @"key" _.cells ) # foreach @"rowKey" @( rowKey :: String, cells :: Array { key :: String, text :: String, status :: [ selected :: {}, unselected :: {} ] } ) sheetRows ) ) # joined @"picked"
    fold @"picked" selectCell
  ) # mvu
    @( cells :: Object String
     , selected :: [ picked :: { name :: String }, none :: {} ]
     , "Formula (e.g. =SUM(A0:A5)*2)" :: String
     )
    orderSheet

headerFace :: String
headerFace = "border: 1px solid #ddd; background: #f4f4f4; padding: 2px 6px; position: sticky; top: 0;"

cellFace :: { key :: String, text :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
cellFace { status } = "border: 1px solid #eee; padding: 2px 6px; min-width: 48px; height: 18px; cursor: cell;"
  <> match { selected: \_ -> " background: #cde;", unselected: \_ -> "" } status
