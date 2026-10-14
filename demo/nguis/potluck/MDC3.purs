module Potluck.MDC3 (potluck) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import Potluck (guestCountLine, invitation, menuLine, menuState, waitingLine)
import PUI (acted, with)
import PUI.Web ((<+>), choice, inCase, text, textAt)
import PUI.Web.MDC3 (body, bodyMedium, group, headlineSmall, list, listItem, segmentedButtonUnpicked, titleMedium)
import QualifiedDo.Semigroupoid as Semigroupoid

potluck :: Effect Unit
potluck =
  body $ Semigroupoid.do
    bodyMedium $ text guestCountLine
    group @"Guests" $ list $
      ( listItem $ Semigroupoid.do
        titleMedium $ textAt @"name"
        segmentedButtonUnpicked @"Dish" @"chosen"
          (choice @"Salad" <+> choice @"Lasagna" <+> choice @"Pavlova") ) # acted @"name"
    ( headlineSmall $ text menuLine ) # inCase @"complete" @( complete :: {}, waiting :: {} ) menuState
    ( bodyMedium $ text waitingLine ) # inCase @"waiting" menuState
  # with
    @{ "Guests" :: Array { name :: String, "Dish" :: [ chosen :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ], unchosen :: {} ] }
     } invitation
