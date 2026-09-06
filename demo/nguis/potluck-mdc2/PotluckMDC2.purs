module PotluckMDC2 (potluckMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PotluckLogic (guestCountLine, guestName, invitation, menuLine)
import PUI (acted, with)
import PUI.Web (choice)
import PUI.Web.HTML (shown, text)
import PUI.Web.MDC2 (body, body2, group, headline6, list, listItem, segmentedButton, subtitle1)
import QualifiedDo.Category as Category

potluckMDC2 :: Effect Unit
potluckMDC2 =
  body $ ( Category.do
      body2 (text guestCountLine) # shown
      group @"Guests" $ list $
          ( listItem $ RecordToRecord.do
              subtitle1 (text guestName)
              segmentedButton @"Dish"
                [ choice @"Salad", choice @"Lasagna", choice @"Pavlova" ] ) # acted @"name"
      headline6 (text menuLine) # shown
  ) # with invitation
