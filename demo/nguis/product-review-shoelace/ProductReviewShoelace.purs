module ProductReviewShoelace (productReviewShoelace) where

import Prelude (Unit, ($), (#))

import Effect (Effect)
import ProductReviewLogic (freshImpression, previewLine, submittedLine)
import PUI (armed, forCase, mvu)
import PUI.Web (choice, shown, text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, button, card, divider, rating, select, textArea, textField, toast, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

productReviewShoelace :: Effect Unit
productReviewShoelace =
  body $
    card $ Semigroupoid.do
      ( Semigroupoid.do
        rating @"Overall rating" {}
        textField @"Headline" {}
        textArea @"Your review" { rows: 4 }
        select @"How long have you owned it?" {}
          [ choice @"less than a month", choice @"1–12 months", choice @"more than a year" ]
        toggleSwitch @"I'd recommend it to a friend" {}
        textField @"Nickname" {}
        divider # shown
      ) # mvu freshImpression
      p (text previewLine) # shown
      button @"Submit review" {} # armed
      toast # forCase @"Submit review" submittedLine
