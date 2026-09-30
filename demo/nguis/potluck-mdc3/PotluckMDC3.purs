module PotluckMDC3 (potluckMDC3) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PotluckLogic (guestCountLine, guestName, invitation, menuLine, menuState, waitingLine)
import PUI (acted, with)
import PUI.Web (choice, shown, shownWhen, text)
import PUI.Web.MDC3 (body, bodyMedium, group, headlineSmall, list, listItem, segmentedButtonUnpicked, titleMedium)
import QualifiedDo.Semigroupoid as Semigroupoid

potluckMDC3 :: Effect Unit
potluckMDC3 =
  body $ ( Semigroupoid.do
    bodyMedium (text guestCountLine) # shown
    group @"Guests" $ list $
      ( listItem $ Semigroupoid.do
        titleMedium (text guestName) # shown
        segmentedButtonUnpicked @"Dish" @"chosen"
          [ choice @"Salad", choice @"Lasagna", choice @"Pavlova" ] ) # acted @"name"
    headlineSmall (text menuLine) # shownWhen @"complete" menuState
    bodyMedium (text waitingLine) # shownWhen @"waiting" menuState
  ) # with invitation
