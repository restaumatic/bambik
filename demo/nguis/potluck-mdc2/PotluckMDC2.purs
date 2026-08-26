module PotluckMDC2 (potluckMDC2) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import Data.Variant.Case (caseText)
import PotluckLogic (guestCount, invitation)
import PUI (acted, field, foreach, projection, projected, with)
import PUI.Web (choice)
import PUI.Web.HTML (shown, body, span, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, headline6, list, listItem, segmentedButton, subtitle1)
import QualifiedDo.Category as Category

potluckMDC2 :: Effect Unit
potluckMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          ( body2 $ RecordToRecord.do
              text @"guestCount" # projected guestCount
              staticText " guests invited — everyone picks one dish; the menu prints once the table is complete." ) # shown
          ( list $
              ( listItem $ RecordToRecord.do
                  subtitle1 (text @"name")
                  segmentedButton @"Dish"
                    [ choice @"Salad", choice @"Lasagna", choice @"Pavlova" ] ) # acted @"name" ) # field @"guests"
          headline6 $ Category.do
            (staticText "On the table: ") # shown
            ( span $ RecordToRecord.do
                text @"name"
                staticText "’s "
                text @"Dish" # projection caseText
                staticText ", " ) # foreach @"name" identity # field @"guests"
      ) # with invitation
