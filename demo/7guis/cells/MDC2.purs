module Cells.MDC2 (cells) where

import Prelude (Unit, (#), ($), (<>), (>>>))

import Cells (columnHeaders, commit, orderSheet, rowLabel, selectCell, selectedLine, sheetRows)
import Data.Variant (match)
import Effect (Effect)
import Foreign.Object (Object)
import PUI (anyOf, anyOfAt, fold, foreach, joined, looped, settled, with)
import PUI.Web (attrWith, clicked, text, textAt, (:=))
import PUI.Web.HTML (div, table, td, tr)
import PUI.Web.MDC2 (body, body1, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid

cells :: Effect Unit
cells =
  body $ Semigroupoid.do
    body1 $ text selectedLine
    filledTextField @"Formula (e.g. =SUM(A0:A5)*2)" # settled commit
    ( div >>> "style" := "overflow: auto; max-height: 420px;" $
      table >>> "style" := "border-collapse: collapse; font-size: 13px;" $ Semigroupoid.do
        tr $ ( td >>> "style" := headerFace $ textAt @"text" ) # foreach @"key" @( key :: String, text :: String ) columnHeaders
        ( tr $ Semigroupoid.do
          td >>> "style" := headerFace $ text rowLabel
          ( clicked @"Cell picked" @"key" $ td >>> attrWith "style" cellFace $ textAt @"text" ) # anyOfAt @"key" @"cells" ) # anyOf @"rowKey"
          @( rowKey :: String
           , cells :: Array { key :: String, text :: String, status :: [ selected :: {}, unselected :: {} ] }
           ) sheetRows ) # joined
    fold @"Cell picked" selectCell
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
