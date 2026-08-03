module TicTacToeMDC2 (ticTacToeMDC2) where

import Prelude (identity, (#), ($), (<>), (>>>), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (displayed, forField, foreach, mvu, forProperty, toCase, updated, with)
import PUI.Web.HTML (atCase, attrWith, body, clicked, div, staticText, text, (:=))
import PUI.Web.MDC2 (button, card, elevation20, headline6)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicTacToeLogic (cells, claimCell, gameOutcome, openingPosition)

ticTacToeMDC2 :: Effect Unit
ticTacToeMDC2 =
  body $
    elevation20 $
      card { caption: "Tic-Tac-Toe" } $ ( Semigroupoid.do
          headline6 ( RecordToRecord.do
              text # forField @"mark" identity
              staticText " wins" ) # atCase @"won" gameOutcome # displayed
          headline6 (staticText "Draw") # atCase @"drawn" gameOutcome # displayed
          headline6 ( RecordToRecord.do
              text # forField @"mark" identity
              staticText " to move" ) # atCase @"toMove" gameOutcome # displayed
          ( ( div >>> "style" := "display: grid; grid-template-columns: repeat(3, 72px); gap: 4px; width: max-content; margin-bottom: 10px;" $
                  ( clicked
                      ( div
                          >>> attrWith "style" (\c -> cellStyle <> if c.win then "background: #a5d6a7;" else "background: #eceff1;")
                          $ text # forProperty @"mark" identity)) # foreach @"key" cells) # toCase @"cellPicked" _.key) # updated (match { cellPicked: claimCell })
          button { label: "New game", icon: "replay" } # with openingPosition # updated (match { clicked: const })
      ) # mvu openingPosition

cellStyle :: String
cellStyle =
  "height: 72px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 40px; font-family: Roboto, sans-serif; cursor: pointer; border-radius: 4px; "
