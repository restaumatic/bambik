module TicTacToeMDC3 (ticTacToeMDC3) where

import Prelude ((#), ($), (<>), (>>>), Unit)

import Data.Variant (match)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (cycled, fold, foreach, joined, with)
import PUI.Web (attrWith, clicked, shownWhen, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, button, headlineSmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicTacToeViewModel (cellMark, cells, claimCell, drawnLine, gameOutcome, newGame, toMoveLine, wonLine)

ticTacToeMDC3 :: Effect Unit
ticTacToeMDC3 =
  body $
    ( Semigroupoid.do
      VariantToRecord.do
        fold @"claimed" claimCell
        fold @"New game" @( board :: Array [ x :: {}, o :: {}, free :: {} ] ) newGame
      headlineSmall (text wonLine) # shownWhen @"won" @( won :: { mark :: [ x :: {}, o :: {}, free :: {} ] }, drawn :: {}, toMove :: { mark :: [ x :: {}, o :: {}, free :: {} ] } ) gameOutcome
      headlineSmall (text drawnLine) # shownWhen @"drawn" gameOutcome
      headlineSmall (text toMoveLine) # shownWhen @"toMove" gameOutcome
      RecordToVariant.do
        ( ( div >>> "style" := "display: grid; grid-template-columns: repeat(3, 72px); gap: 4px; width: max-content; margin-bottom: 10px;" $
          clicked @"claimed" _.key ( div >>> attrWith "style" cellFace $ text cellMark ) # foreach @"key" @( key :: String, mark :: [ x :: {}, o :: {}, free :: {} ], line :: [ winning :: {}, plain :: {} ] ) cells ) ) # joined @"claimed"
        button @"New game" { icon: "replay" } # with {}
    ) # cycled # with (."New game" {})

cellFace :: { key :: String, mark :: [ x :: {}, o :: {}, free :: {} ], line :: [ winning :: {}, plain :: {} ] } -> String
cellFace { line } = cellStyle <> match { winning: \_ -> "background: #a5d6a7;", plain: \_ -> "background: #eceff1;" } line

cellStyle :: String
cellStyle =
  "height: 72px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 40px; font-family: Roboto, sans-serif; cursor: pointer; border-radius: 4px; "
