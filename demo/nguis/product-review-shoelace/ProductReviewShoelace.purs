module ProductReviewShoelace (productReviewShoelace) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import ProductReviewLogic (freshImpression, presentReview, submittedLine)
import PUI (armed, forCases, mvu, required, settled)
import PUI.Web (choice)
import PUI.Web.HTML (shown, body, p, staticText, text)
import PUI.Web.Shoelace (button, card, divider, rating, select, textArea, textField, toast, toggleSwitch)
import QualifiedDo.Category as Category

productReviewShoelace :: Effect Unit
productReviewShoelace =
  body $
    card $ Category.do
      ( Category.do
          rating @"Overall rating" {}
          textField @"Headline" {}
          textArea @"Your review" { rows: 4 }
          select @"How long have you owned it?" {}
            [ choice @"less than a month", choice @"1–12 months", choice @"more than a year" ] # required
          toggleSwitch @"I'd recommend it to a friend" {}
          textField @"Nickname" {}
          divider # shown
      ) # settled presentReview # mvu freshImpression
      ( p $ RecordToRecord.do
          staticText "Preview: "
          text @"starsText"
          text @"quoteText"
          staticText " · owned "
          text @"ownedText"
          text @"recommendText" ) # shown
      button @"Submit review" {} # armed
      toast # forCases (match { "Submit review": submittedLine })
