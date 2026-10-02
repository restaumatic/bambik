module PotluckMDC2 (potluckMDC2) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PotluckViewModel (guestCountLine, guestName, invitation, menuLine, menuState, waitingLine)
import PUI (acted, with)
import PUI.Web ((<+>), choice, shown, shownWhen, text)
import PUI.Web.MDC2 (body, body2, group, headline6, list, listItem, segmentedButtonUnpicked, subtitle1)
import QualifiedDo.Semigroupoid as Semigroupoid

potluckMDC2 :: Effect Unit
potluckMDC2 =
  body $ ( Semigroupoid.do
    body2 (text guestCountLine) # shown
    group @"Guests" $ list $
      ( listItem $ Semigroupoid.do
        subtitle1 (text guestName) # shown
        segmentedButtonUnpicked @"Dish" @"chosen"
          (choice @"Salad" <+> choice @"Lasagna" <+> choice @"Pavlova") ) # acted @"name"
    headline6 (text menuLine) # shownWhen @"complete" @( complete :: { dishes :: Array { name :: String, dish :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } }, waiting :: { remaining :: Array String } ) menuState
    body2 (text waitingLine) # shownWhen @"waiting" menuState
  ) # with
    @( "Guests" :: Array { name :: String, "Dish" :: [ chosen :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ], unchosen :: {} ] }
     )
    invitation
