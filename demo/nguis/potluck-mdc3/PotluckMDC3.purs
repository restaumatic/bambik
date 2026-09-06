module PotluckMDC3 (potluckMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PotluckLogic (guestCountLine, guestName, invitation, menuLine)
import PUI (acted, with)
import PUI.Web (choice)
import PUI.Web.HTML (shown, text)
import PUI.Web.MDC3 (body, bodyMedium, group, headlineSmall, list, listItem, segmentedButton, titleMedium)
import QualifiedDo.Category as Category

potluckMDC3 :: Effect Unit
potluckMDC3 =
  body $ ( Category.do
    bodyMedium (text guestCountLine) # shown
    group @"Guests" $ list $
      ( listItem $ RecordToRecord.do
        titleMedium (text guestName)
        segmentedButton @"Dish"
          [ choice @"Salad", choice @"Lasagna", choice @"Pavlova" ] ) # acted @"name"
    headlineSmall (text menuLine) # shown
  ) # with invitation
