module ProductReviewShoelace (productReviewShoelace) where

import Prelude (identity, Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant.Case (caseText)
import Effect (Effect)
import ProductReviewLogic (freshImpression, headlineQuote, recommendNote, starGlyphs, submittedLine)
import PUI (forCase, projection, mvu, projected, required)
import PUI.Web (choice)
import PUI.Web.HTML (shownAs, body, p, staticText, text)
import PUI.Web.Shoelace (button, card, divider, rating, select, textArea, textField, toast, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

productReviewShoelace :: Effect Unit
productReviewShoelace =
  body $
    card $ Semigroupoid.do
      ( RecordToRecord.do
          rating @"Overall rating" {}
          textField @"Headline" {}
          textArea @"Your review" { rows: 4 }
          select @"How long have you owned it?" {}
            [ choice @"less than a month", choice @"1–12 months", choice @"more than a year" ] # required
          toggleSwitch @"I'd recommend it to a friend" {}
          textField @"Nickname" {}
          divider
      ) # mvu freshImpression
      shownAs identity ( p $ RecordToRecord.do
          staticText "Preview: "
          text @"Overall rating" # projection starGlyphs
          text @"Headline" # projection headlineQuote
          staticText " · owned "
          text @"How long have you owned it?" # projection caseText
          text @"recommendNote" # projected recommendNote )
      button @"Submit review" {}
      toast # forCase @"Submit review" submittedLine
