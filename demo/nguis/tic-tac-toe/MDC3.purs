module TicTacToe.MDC3 (ticTacToe) where

import Prelude ((#), ($), (<>), (>>>), Unit)

import Data.Variant (match)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (anyOf, fold, joined, looped, reset, with)
import PUI.Web (attrWith, inCase, staticText, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, button', headlineSmall, tile)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicTacToe (cellMark, cells, claimCell, gameOutcome, openingPosition, toMoveLine, wonLine)

ticTacToe :: Effect Unit
ticTacToe =
  body $ Semigroupoid.do
    ( headlineSmall $ text wonLine ) # inCase @"won" @( won :: {}, drawn :: {}, toMove :: {} ) gameOutcome
    ( headlineSmall $ staticText "Draw" ) # inCase @"drawn" gameOutcome
    ( headlineSmall $ text toMoveLine ) # inCase @"toMove" gameOutcome
    RecordToVariant.do
      ( div >>> "style" := "display: grid; grid-template-columns: repeat(3, 72px); gap: 4px; width: max-content; margin-bottom: 10px;" $
        ( attrWith "style" cellFace $ tile @"Cell claimed" @"key" $ text cellMark ) # anyOf @"key"
          @( key :: String
           , mark :: [ x :: {}, o :: {}, free :: {} ]
           , line :: [ winning :: {}, plain :: {} ]
           ) cells ) # joined @"Cell claimed"
      button' @"New game" { icon: "replay" }
    VariantToRecord.do
      fold @"Cell claimed" claimCell
      reset @"New game" openingPosition
  # looped @( board :: Array [ x :: {}, o :: {}, free :: {} ] ) # with openingPosition

cellFace :: { key :: String, mark :: [ x :: {}, o :: {}, free :: {} ], line :: [ winning :: {}, plain :: {} ] } -> String
cellFace { line } = cellStyle <> match { winning: \_ -> "background: #a5d6a7;", plain: \_ -> "background: #eceff1;" } line

cellStyle :: String
cellStyle =
  "height: 72px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 40px; font-family: Roboto, sans-serif; cursor: pointer; border-radius: 4px; "
