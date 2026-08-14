module ProductReviewShoelace (productReviewShoelace) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import ProductReviewLogic (freshImpression, headlineQuote, ownedText, recommendNote, starGlyphs, submittedLine)
import PUI (forCase, projection, mvu, projected, required, tapped)
import PUI.Web.HTML (body, p, staticText, text)
import PUI.Web.Shoelace (button, card, divider, rating, select, textArea, textField, toast, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

productReviewShoelace :: Effect Unit
productReviewShoelace =
  body $
    card { caption: "Review: Astra Moka Espresso Machine" } $ Semigroupoid.do
      ( RecordToRecord.do
          rating @"Overall rating" {}
          textField @"Headline" {}
          textArea @"Your review" { rows: 4 }
          select @"How long have you owned it?" {}
            [ { value: .underMonth {}, label: "Less than a month" }
            , { value: .underYear {}, label: "1–12 months" }
            , { value: .overYear {}, label: "More than a year" }
            ] # required
          toggleSwitch @"I'd recommend it to a friend" {}
          textField @"Nickname" {}
          divider
      ) # mvu freshImpression
      p ( RecordToRecord.do
          staticText "Preview: "
          text @"Overall rating" # projection starGlyphs
          text @"Headline" # projection headlineQuote
          staticText " · owned "
          text @"How long have you owned it?" # projection ownedText
          text @"recommendNote" # projected recommendNote ) # tapped
      button @"Submit review" {}
      toast # forCase @"Submit review" submittedLine
