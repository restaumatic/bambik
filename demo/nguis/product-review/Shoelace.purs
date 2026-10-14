module ProductReview.Shoelace (productReview) where

import Prelude (($), (#), identity, Unit)

import Effect (Effect)
import ProductReview (freshImpression, previewLine, submitReview)
import PUI (action, atCase, looped, with)
import PUI.Web ((<+>), choice, text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, button, card, divider, indeterminateLinearProgress, rating, select, textArea, textField, toast, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

productReview :: Effect Unit
productReview =
  body $ Semigroupoid.do
    rating @"Overall rating"
    textField @"Headline"
    textArea @"Your review" { rows: 4 }
    select @"How long have you owned it?"
      (choice @"less than a month" <+> choice @"1–12 months" <+> choice @"more than a year")
    toggleSwitch @"I'd recommend it to a friend"
    textField @"Nickname"
    divider $ card $ p $ text previewLine
    button @"Submit review"
    indeterminateLinearProgress # action submitReview # atCase @"Submit review"
    toast @"Review submitted" identity
  # looped
    @( "Overall rating" :: { current :: Number, max :: Int }
     , "Headline" :: String
     , "Your review" :: String
     , "How long have you owned it?" :: [ "less than a month" :: {}, "1–12 months" :: {}, "more than a year" :: {} ]
     , "I'd recommend it to a friend" :: Boolean
     , "Nickname" :: String
     ) # with freshImpression
