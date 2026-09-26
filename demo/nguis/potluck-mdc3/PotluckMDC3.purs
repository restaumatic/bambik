module PotluckMDC3 (potluckMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PotluckLogic (guestCountLine, guestName, invitation, menuLine, menuState, waitingLine)
import PUI (acted, optional, with)
import PUI.Web (choice, shown, shownWhen, text)
import PUI.Web.MDC3 (body, bodyMedium, group, headlineSmall, list, listItem, segmentedButton, titleMedium)
import QualifiedDo.Semigroupoid as Semigroupoid

potluckMDC3 :: Effect Unit
potluckMDC3 =
  body $ ( Semigroupoid.do
    bodyMedium (text guestCountLine) # shown
    group @"Guests" $ list $
      ( listItem $ RecordToRecord.do
        titleMedium (text guestName)
        segmentedButton @"Dish" (optional @"chosen")
          [ choice @"Salad", choice @"Lasagna", choice @"Pavlova" ] ) # acted @"name"
    headlineSmall (text menuLine) # shownWhen @"complete" menuState
    bodyMedium (text waitingLine) # shownWhen @"waiting" menuState
  ) # with invitation
