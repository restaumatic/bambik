module TicTacToeMDC2 (ticTacToeMDC2) where

import Prelude ((#), ($), (<>), (>>>), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, updated, with)
import PUI.Web (attrWith, clicked, shownWhen, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC2 (body, button, headline6)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicTacToeLogic (cellMark, cells, claimCell, drawnLine, gameOutcome, openingPosition, toMoveLine, wonLine)

ticTacToeMDC2 :: Effect Unit
ticTacToeMDC2 =
  body $
    ( Semigroupoid.do
      headline6 (text wonLine) # shownWhen @"won" gameOutcome
      headline6 (text drawnLine) # shownWhen @"drawn" gameOutcome
      headline6 (text toMoveLine) # shownWhen @"toMove" gameOutcome
      ( ( div >>> "style" := "display: grid; grid-template-columns: repeat(3, 72px); gap: 4px; width: max-content; margin-bottom: 10px;" $
        clicked @"claimed" _.key ( div >>> attrWith "style" cellFace $ text cellMark ) # foreach @"key" cells ) ) # updated (match { claimed: claimCell })
      button @"New game" { icon: "replay" } # with openingPosition # updated (match { "New game": const })
    ) # mvu openingPosition

cellFace :: forall r1. { mark :: [ x :: {}, o :: {}, free :: {} ], line :: [ winning :: {}, plain :: {} ] | r1 } -> String
cellFace { line } = cellStyle <> match { winning: \_ -> "background: #a5d6a7;", plain: \_ -> "background: #eceff1;" } line

cellStyle :: String
cellStyle =
  "height: 72px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 40px; font-family: Roboto, sans-serif; cursor: pointer; border-radius: 4px; "
