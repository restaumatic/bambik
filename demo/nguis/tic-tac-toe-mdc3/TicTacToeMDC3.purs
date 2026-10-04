module TicTacToeMDC3 (ticTacToeMDC3) where

import Prelude ((#), ($), (<>), (>>>), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, updated, with)
import PUI.Web (attrWith, clicked, shownWhen, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, button, headlineSmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicTacToeViewModel (cellMark, cells, claimCell, drawnLine, gameOutcome, openingPosition, toMoveLine, wonLine)

ticTacToeMDC3 :: Effect Unit
ticTacToeMDC3 =
  body $
    ( Semigroupoid.do
      headlineSmall (text wonLine) # shownWhen @"won" @( won :: { mark :: [ x :: {}, o :: {}, free :: {} ] }, drawn :: {}, toMove :: { mark :: [ x :: {}, o :: {}, free :: {} ] } ) gameOutcome
      headlineSmall (text drawnLine) # shownWhen @"drawn" gameOutcome
      headlineSmall (text toMoveLine) # shownWhen @"toMove" gameOutcome
      ( ( div >>> "style" := "display: grid; grid-template-columns: repeat(3, 72px); gap: 4px; width: max-content; margin-bottom: 10px;" $
        clicked @"claimed" _.key ( div >>> attrWith "style" cellFace $ text cellMark ) # foreach @"key" @( key :: String, mark :: [ x :: {}, o :: {}, free :: {} ], line :: [ winning :: {}, plain :: {} ] ) cells ) ) # updated (match { claimed: claimCell })
      button @"New game" { icon: "replay" } # with @( board :: Array [ x :: {}, o :: {}, free :: {} ] ) openingPosition # updated (match { "New game": const })
    ) # mvu @( board :: Array [ x :: {}, o :: {}, free :: {} ] ) openingPosition

cellFace :: { key :: String, mark :: [ x :: {}, o :: {}, free :: {} ], line :: [ winning :: {}, plain :: {} ] } -> String
cellFace { line } = cellStyle <> match { winning: \_ -> "background: #a5d6a7;", plain: \_ -> "background: #eceff1;" } line

cellStyle :: String
cellStyle =
  "height: 72px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 40px; font-family: Roboto, sans-serif; cursor: pointer; border-radius: 4px; "
