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
          rating @"stars" { label: "Overall rating" }
          textField @"headline" {}
          textArea @"review" { label: "Your review", rows: 4 }
          select @"owned" { label: "How long have you owned it?" }
            [ { value: .underMonth {}, label: "Less than a month" }
            , { value: .underYear {}, label: "1–12 months" }
            , { value: .overYear {}, label: "More than a year" }
            ] # required
          toggleSwitch @"recommend" { label: "I'd recommend it to a friend" }
          textField @"nickname" {}
          divider
      ) # mvu freshImpression
      p ( RecordToRecord.do
          staticText "Preview: "
          text @"stars" # projection starGlyphs
          text @"headline" # projection headlineQuote
          staticText " · owned "
          text @"owned" # projection ownedText
          text @"recommendNote" # projected recommendNote ) # tapped
      button @"submitted" { label: "Submit review" }
      toast # forCase @"submitted" submittedLine
