module TicTacToe.MDC3 (ticTacToe) where

import Prelude ((#), ($), (<>), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (foldInto, gatheredOf, looped, reset, stepped, with)
import PUI.Web (attrWith, inCase, staticText, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, button', headlineSmall, tile)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicTacToe (cellMark, cells, claimCell, gameOutcome, openingPosition, toMoveLine, wonLine)

ticTacToe :: Effect Unit
ticTacToe =
  body $ Semigroupoid.do
    ( headlineSmall $ text wonLine ) # inCase @"won" gameOutcome
    ( headlineSmall $ staticText "Draw" ) # inCase @"drawn" gameOutcome
    ( headlineSmall $ text toMoveLine ) # inCase @"toMove" gameOutcome
    ( Semigroupoid.do
      RecordToVariant.do
        div >>> "style" := "display: grid; grid-template-columns: repeat(3, 72px); gap: 4px; width: max-content; margin-bottom: 10px;" $
          ( tile @"Cell claimed" @"key" $ text cellMark ) # attrWith "style" cellFace # gatheredOf @"key" cells
        button' @"New game" { icon: "replay" }
      VariantToRecord.do
        foldInto @"Cell claimed" claimCell
        reset @"New game" openingPosition ) # stepped
  # looped # with openingPosition

cellFace
  :: { key :: Int
     , line :: [ plain :: {}, winning :: {} ]
     , mark :: [ free :: {}, o :: {}, x :: {} ]
     }
  -> String
cellFace { line } = cellStyle <> match { winning: \_ -> "background: #a5d6a7;", plain: \_ -> "background: #eceff1;" } line

cellStyle :: String
cellStyle =
  "height: 72px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 40px; font-family: Roboto, sans-serif; cursor: pointer; border-radius: 4px; "
