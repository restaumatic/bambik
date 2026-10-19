module Potluck.MDC3 (potluck) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import Potluck (guestCountLine, invitation, menuLine, menuState, waitingLine)
import PUI (acted, with)
import PUI.Web (inCase, text, textAt)
import PUI.Web.MDC3 (body, bodyMedium, group, headlineSmall, list, listItem, segment, segmentedButtonUnpicked, titleMedium)
import QualifiedDo.Semigroupoid as Semigroupoid

potluck :: Effect Unit
potluck =
  body $ Semigroupoid.do
    bodyMedium $ text guestCountLine
    group @"Guests" $ list $
      ( listItem $ Semigroupoid.do
        titleMedium $ textAt @"name"
        segmentedButtonUnpicked @"Dish" @"chosen" $ VariantToVariant.do
          segment @"Salad"
          segment @"Lasagna"
          segment @"Pavlova" ) # acted @"name"
    ( headlineSmall $ text menuLine ) # inCase @"complete" menuState
    ( bodyMedium $ text waitingLine ) # inCase @"waiting" menuState
  # with invitation
