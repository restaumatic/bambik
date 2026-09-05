module PotluckMDC3 (potluckMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PotluckLogic (guestCountLine, guestName, invitation, menuLine)
import PUI (acted, foreach, with)
import PUI.Web (choice)
import PUI.Web.HTML (shown, body, span, staticText, text)
import PUI.Web.MDC3 (bodyMedium, elevation5, group, headlineSmall, list, listItem, segmentedButton, titleMedium)
import QualifiedDo.Category as Category

potluckMDC3 :: Effect Unit
potluckMDC3 =
  body $
    elevation5 $ ( Category.do
        bodyMedium (text guestCountLine) # shown
        group @"Guests" $ list $
            ( listItem $ RecordToRecord.do
                titleMedium (text guestName)
                segmentedButton @"Dish"
                  [ choice @"Salad", choice @"Lasagna", choice @"Pavlova" ] ) # acted @"name"
        headlineSmall $ Category.do
          (staticText "On the table: ") # shown
          ( span $ text menuLine ) # foreach @"name" _."Guests"
    ) # with invitation
