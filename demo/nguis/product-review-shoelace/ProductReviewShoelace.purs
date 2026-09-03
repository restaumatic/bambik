module ProductReviewShoelace (productReviewShoelace) where

import Prelude (Unit, ($), (#))

import Effect (Effect)
import ProductReviewLogic (freshImpression, previewLine, submittedLine)
import PUI (armed, forCase, mvu, required)
import PUI.Web (choice)
import PUI.Web.HTML (shown, body, p, text)
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
      ) # mvu freshImpression
      p (text previewLine) # shown
      button @"Submit review" {} # armed
      toast # forCase @"Submit review" submittedLine
