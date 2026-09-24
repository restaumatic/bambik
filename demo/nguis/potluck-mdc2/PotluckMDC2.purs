module PotluckMDC2 (potluckMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PotluckLogic (guestCountLine, guestName, invitation, menuLine, menuState, waitingLine)
import PUI (acted, optional, with)
import PUI.Web (choice, shown, shownWhen, text)
import PUI.Web.MDC2 (body, body2, group, headline6, list, listItem, segmentedButton, subtitle1)
import QualifiedDo.Semigroupoid as Semigroupoid

potluckMDC2 :: Effect Unit
potluckMDC2 =
  body $ ( Semigroupoid.do
    body2 (text guestCountLine) # shown
    group @"Guests" $ list $
      ( listItem $ RecordToRecord.do
        subtitle1 (text guestName)
        segmentedButton @"Dish"
          [ choice @"Salad", choice @"Lasagna", choice @"Pavlova" ] # optional @"chosen" @"unchosen" ) # acted @"name"
    headline6 (text menuLine) # shownWhen @"complete" menuState
    body2 (text waitingLine) # shownWhen @"waiting" menuState
  ) # with invitation
