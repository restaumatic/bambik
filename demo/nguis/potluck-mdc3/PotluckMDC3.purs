module PotluckMDC3 (potluckMDC3) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import Data.Variant.Case (caseText)
import PotluckLogic (guestCount, invitation)
import PUI (acted, field, foreach, projection, with)
import PUI.Web (choice)
import PUI.Web.HTML (shown, body, span, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, headlineSmall, list, listItem, segmentedButton, titleMedium)
import QualifiedDo.Category as Category

potluckMDC3 :: Effect Unit
potluckMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( bodyMedium $ RecordToRecord.do
              text @"guests" # projection guestCount
              staticText " guests invited — everyone picks one dish; the menu prints once the table is complete." ) # shown
          ( list $
              ( listItem $ RecordToRecord.do
                  titleMedium (text @"name")
                  segmentedButton @"Dish"
                    [ choice @"Salad", choice @"Lasagna", choice @"Pavlova" ] ) # acted @"name" ) # field @"guests"
          headlineSmall $ Category.do
            (staticText "On the table: ") # shown
            ( span $ RecordToRecord.do
                text @"name"
                staticText "’s "
                text @"Dish" # projection caseText
                staticText ", " ) # foreach @"name" identity # field @"guests"
      ) # with invitation
