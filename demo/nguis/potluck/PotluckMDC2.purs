module PotluckMDC2 (potluckMDC2) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PotluckViewModel (guestCountLine, guestName, invitation, menuLine, menuState, waitingLine)
import PUI (acted, prefixed, with)
import PUI.Web ((<+>), choice, pane, text)
import PUI.Web.MDC2 (body, body2, group, headline6, list, listItem, segmentedButtonUnpicked, subtitle1)
import QualifiedDo.Semigroupoid as Semigroupoid

potluckMDC2 :: Effect Unit
potluckMDC2 =
  body $ Semigroupoid.do
    prefixed (body2 (text guestCountLine)) $ group @"Guests" $ list $
      ( listItem $ prefixed (subtitle1 (text guestName)) $ segmentedButtonUnpicked @"Dish" @"chosen"
        (choice @"Salad" <+> choice @"Lasagna" <+> choice @"Pavlova") ) # acted @"name"
    prefixed (headline6 (text menuLine) # pane @"complete"
      @( complete :: { dishes :: Array { name :: String, dish :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } }
       , waiting :: { remaining :: Array String }
       ) menuState ) $ body2 (text waitingLine) # pane @"waiting" menuState
  # with
    @{ "Guests" :: Array { name :: String, "Dish" :: [ chosen :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ], unchosen :: {} ] }
     }
    invitation
