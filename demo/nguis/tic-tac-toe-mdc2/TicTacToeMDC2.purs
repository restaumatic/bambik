module TicTacToeMDC2 (ticTacToeMDC2) where

import Prelude ((#), ($), (<>), (>>>), Unit, const)

import Data.Variant (match)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (blankStatus, fold, foreach, joined, looped, with)
import PUI.Web (attrWith, clicked, shownWhen, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC2 (body, button, headline6)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicTacToeViewModel (cellMark, cells, claimCell, drawnLine, gameOutcome, openingPosition, toMoveLine, wonLine)

ticTacToeMDC2 :: Effect Unit
ticTacToeMDC2 =
  body $ Semigroupoid.do
    headline6 (text wonLine) # shownWhen @"won"
      @( won :: { mark :: [ x :: {}, o :: {}, free :: {} ] }
       , drawn :: {}
       , toMove :: { mark :: [ x :: {}, o :: {}, free :: {} ] }
       ) gameOutcome
    headline6 (text drawnLine) # shownWhen @"drawn" gameOutcome
    headline6 (text toMoveLine) # shownWhen @"toMove" gameOutcome
    RecordToVariant.do
      ( ( div >>> "style" := "display: grid; grid-template-columns: repeat(3, 72px); gap: 4px; width: max-content; margin-bottom: 10px;" $
        clicked @"Cell claimed" @"key" ( div >>> attrWith "style" cellFace $ text cellMark ) # foreach @"key"
          @( key :: String
           , mark :: [ x :: {}, o :: {}, free :: {} ]
           , line :: [ winning :: {}, plain :: {} ]
           ) cells ) ) # joined @"Cell claimed"
      button @"New game" { icon: "replay" }
    VariantToRecord.do
      blankStatus @"Cell claimed" # fold claimCell
      blankStatus @"New game" # fold (const openingPosition)
  # looped @( board :: Array [ x :: {}, o :: {}, free :: {} ] ) # with openingPosition

cellFace :: { key :: String, mark :: [ x :: {}, o :: {}, free :: {} ], line :: [ winning :: {}, plain :: {} ] } -> String
cellFace { line } = cellStyle <> match { winning: \_ -> "background: #a5d6a7;", plain: \_ -> "background: #eceff1;" } line

cellStyle :: String
cellStyle =
  "height: 72px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 40px; font-family: Roboto, sans-serif; cursor: pointer; border-radius: 4px; "
