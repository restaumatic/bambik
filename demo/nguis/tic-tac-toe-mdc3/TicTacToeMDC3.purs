module TicTacToeMDC3 (ticTacToeMDC3) where

import Prelude (identity, (#), ($), (<>), (>>>), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, displayed, forProperty, foreach, mvu, toCase, updated)
import PUI.Web.HTML (providedCase, attrWith, body, clicked, div, staticText, text, (:=))
import PUI.Web.MDC3 (button, card, elevation5, headlineSmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicTacToeLogic (cells, claimCell, gameOutcome, openingPosition)

ticTacToeMDC3 :: Effect Unit
ticTacToeMDC3 =
  body $
    elevation5 $
      card { caption: "Tic-Tac-Toe" } $ ( Semigroupoid.do
          headlineSmall ( RecordToRecord.do
              text @"mark"
              staticText " wins" ) # providedCase @"won" gameOutcome # displayed
          headlineSmall (staticText "Draw") # providedCase @"drawn" gameOutcome # displayed
          headlineSmall ( RecordToRecord.do
              text @"mark"
              staticText " to move" ) # providedCase @"toMove" gameOutcome # displayed
          ( ( div >>> "style" := "display: grid; grid-template-columns: repeat(3, 72px); gap: 4px; width: max-content; margin-bottom: 10px;" $
                  ( clicked
                      ( div
                          >>> attrWith "style" cellFace
                          $ text @"mark" # forProperty identity)) # foreach @"key" cells) # toCase @"cellPicked" _.key) # updated (match { cellPicked: claimCell })
          announce openingPosition >>> button @"newGame" { label: "New game", icon: "replay" } # updated (match { newGame: const })
      ) # mvu openingPosition
cellFace :: { mark :: String, win :: Boolean } -> String
cellFace { win } = cellStyle <> if win then "background: #a5d6a7;" else "background: #eceff1;"

cellStyle :: String
cellStyle =
  "height: 72px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 40px; font-family: Roboto, sans-serif; cursor: pointer; border-radius: 4px; "
