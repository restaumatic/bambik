module TicTacToe.MDC2 (ticTacToe) where

import Prelude ((#), ($), (<>), (>>>), Unit)

import Data.Variant (match)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (anyOf, fold, joined, looped, reset, unshown, with)
import PUI.Web (attrWith, clicked, inCase, staticText, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC2 (body, button', headline6)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicTacToe (cellMark, cells, claimCell, gameOutcome, openingPosition, toMoveLine, wonLine)

ticTacToe :: Effect Unit
ticTacToe =
  body $ Semigroupoid.do
    headline6 (text wonLine) # inCase @"won" @( won :: {}, drawn :: {}, toMove :: {} ) gameOutcome
    headline6 (staticText "Draw") # inCase @"drawn" gameOutcome
    headline6 (text toMoveLine) # inCase @"toMove" gameOutcome
    RecordToVariant.do
      ( div >>> "style" := "display: grid; grid-template-columns: repeat(3, 72px); gap: 4px; width: max-content; margin-bottom: 10px;" $
        clicked @"Cell claimed" @"key" ( div >>> attrWith "style" cellFace $ text cellMark ) # anyOf @"key"
          @( key :: String
           , mark :: [ x :: {}, o :: {}, free :: {} ]
           , line :: [ winning :: {}, plain :: {} ]
           ) cells ) # joined @"Cell claimed"
      button' @"New game" { icon: "replay" }
    VariantToRecord.do
      unshown @"Cell claimed" # fold claimCell
      unshown @"New game" # reset openingPosition
  # looped @( board :: Array [ x :: {}, o :: {}, free :: {} ] ) # with openingPosition

cellFace :: { key :: String, mark :: [ x :: {}, o :: {}, free :: {} ], line :: [ winning :: {}, plain :: {} ] } -> String
cellFace { line } = cellStyle <> match { winning: \_ -> "background: #a5d6a7;", plain: \_ -> "background: #eceff1;" } line

cellStyle :: String
cellStyle =
  "height: 72px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 40px; font-family: Roboto, sans-serif; cursor: pointer; border-radius: 4px; "
