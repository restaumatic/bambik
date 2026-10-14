module ProductReview.Shoelace (productReview) where

import Prelude (($), (#), identity, Unit)

import Effect (Effect)
import ProductReview (freshImpression, previewLine, submittedLine)
import PUI (looped, with)
import PUI.Web ((<+>), choice, text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, button, card, divider, rating, select, textArea, textField, toast, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

productReview :: Effect Unit
productReview =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      rating @"Overall rating"
      textField @"Headline"
      textArea @"Your review" { rows: 4 }
      select @"How long have you owned it?"
        (choice @"less than a month" <+> choice @"1–12 months" <+> choice @"more than a year")
      toggleSwitch @"I'd recommend it to a friend"
      textField @"Nickname"
    ) # looped
      @( "Overall rating" :: { current :: Number, max :: Int }
       , "Headline" :: String
       , "Your review" :: String
       , "How long have you owned it?" :: [ "less than a month" :: {}, "1–12 months" :: {}, "more than a year" :: {} ]
       , "I'd recommend it to a friend" :: Boolean
       , "Nickname" :: String
       ) # with freshImpression
    divider $ card $ p $ text previewLine
    button @"Submit review"
    toast @"Submit review" submittedLine identity
