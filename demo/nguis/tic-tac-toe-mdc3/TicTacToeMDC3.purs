module TicTacToeMDC3 (ticTacToeMDC3) where

import Prelude ((#), ($), (<>), (>>>), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (forProperty, foreach, mvu, toCase, updated, with)
import PUI.Web.HTML (shownCase, attrWith, body, clicked, div, staticText, text, (:=))
import PUI.Web.MDC3 (button, card, elevation5, headlineSmall)
import QualifiedDo.Category as Category
import TicTacToeLogic (cells, claimCell, gameOutcome, openingPosition)

ticTacToeMDC3 :: Effect Unit
ticTacToeMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( headlineSmall $ RecordToRecord.do
              text @"mark"
              staticText " wins" ) # shownCase @"won" gameOutcome
          (headlineSmall (staticText "Draw")) # shownCase @"drawn" gameOutcome
          ( headlineSmall $ RecordToRecord.do
              text @"mark"
              staticText " to move" ) # shownCase @"toMove" gameOutcome
          ( ( div >>> "style" := "display: grid; grid-template-columns: repeat(3, 72px); gap: 4px; width: max-content; margin-bottom: 10px;" $
                  ( clicked
                      ( div
                          >>> attrWith "style" cellFace
                          $ text @"mark" # forProperty )) # foreach @"key" cells ) # toCase @"cellPicked" _.key ) # updated (match { cellPicked: claimCell })
          button @"New game" { icon: "replay" } # with openingPosition # updated (match { "New game": const })
      ) # mvu openingPosition
cellFace :: { mark :: String, win :: Boolean } -> String
cellFace { win } = cellStyle <> if win then "background: #a5d6a7;" else "background: #eceff1;"

cellStyle :: String
cellStyle =
  "height: 72px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 40px; font-family: Roboto, sans-serif; cursor: pointer; border-radius: 4px; "
