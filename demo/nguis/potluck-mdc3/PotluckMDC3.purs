module PotluckMDC3 (potluckMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PotluckLogic (invitation, menu)
import PUI (acted, field, foreach, forProperty, with)
import PUI.Web (choice)
import PUI.Web.HTML (shown, body, span, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, headlineSmall, list, listItem, segmentedButton, titleMedium)
import QualifiedDo.Category as Category

potluckMDC3 :: Effect Unit
potluckMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          bodyMedium (text @"guestCountLine") # shown
          ( list $
              ( listItem $ RecordToRecord.do
                  titleMedium (text @"name")
                  segmentedButton @"Dish"
                    [ choice @"Salad", choice @"Lasagna", choice @"Pavlova" ] ) # acted @"name" ) # field @"guests"
          headlineSmall $ Category.do
            (staticText "On the table: ") # shown
            ( span $ text @"menuLine" # forProperty ) # foreach @"name" menu # field @"guests"
      ) # with invitation
