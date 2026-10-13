module Potluck.MDC2 (potluck) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import Potluck (guestCountLine, guestName, invitation, menuLine, menuState, waitingLine)
import PUI (acted, with)
import PUI.Web ((<+>), choice, inCase, text)
import PUI.Web.MDC2 (body, body2, group, headline6, list, listItem, segmentedButtonUnpicked, subtitle1)
import QualifiedDo.Semigroupoid as Semigroupoid

potluck :: Effect Unit
potluck =
  body $ Semigroupoid.do
    body2 (text guestCountLine)
    group @"Guests" $ list $
      ( listItem $ Semigroupoid.do
        subtitle1 (text guestName)
        segmentedButtonUnpicked @"Dish" @"chosen"
          (choice @"Salad" <+> choice @"Lasagna" <+> choice @"Pavlova") ) # acted @"name"
    headline6 (text menuLine) # inCase @"complete" @( complete :: {}, waiting :: {} ) menuState
    body2 (text waitingLine) # inCase @"waiting" menuState
  # with
    @{ "Guests" :: Array { name :: String, "Dish" :: [ chosen :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ], unchosen :: {} ] }
     }
    invitation
