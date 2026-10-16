module ProductReview.Shoelace (productReview) where

import Prelude (($), (#), identity, Unit)

import Effect (Effect)
import ProductReview (freshImpression, previewLine, submitReview)
import PUI (action, looped, with)
import PUI.Web ((<+>), choice, text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, button, card, divider, indeterminateLinearProgress, rating, select, textArea, textField, toast, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

productReview :: Effect Unit
productReview =
  body $ Semigroupoid.do
    rating @"Overall rating"
    textField @"Headline"
    textArea @"Review" { rows: 4 }
    select @"Owned for"
      (choice @"Under a month" <+> choice @"Up to a year" <+> choice @"Over a year")
    toggleSwitch @"Recommend to a friend"
    textField @"Nickname"
    divider $ card $ p $ text previewLine
    button @"Submit review"
    indeterminateLinearProgress # action @"Submit review" submitReview
    toast @"Review submitted" identity
  # looped # with freshImpression
