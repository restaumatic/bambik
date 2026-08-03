module ProductReviewShoelace (productReviewShoelace) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import ProductReviewLogic (freshImpression, headlineQuote, ownedText, recommendNote, starGlyphs, submittedLine)
import PUI (asCase, asField, forCase, forField, mvu, projected, required, tapped)
import PUI.Web.HTML (body, p, staticText, text)
import PUI.Web.Shoelace (button, card, divider, rating, select, textArea, textField, toast, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

productReviewShoelace :: Effect Unit
productReviewShoelace =
  body $
    card { caption: "Review: Astra Moka Espresso Machine" } $ Semigroupoid.do
      ( RecordToRecord.do
          rating { label: "Overall rating" } # asField @"stars"
          textField { label: "Headline" } # asField @"headline"
          textArea { label: "Your review", rows: 4 } # asField @"review"
          select { label: "How long have you owned it?" }
            [ { value: .underMonth {}, label: "Less than a month" }
            , { value: .underYear {}, label: "1–12 months" }
            , { value: .overYear {}, label: "More than a year" }
            ] # required # asField @"owned"
          toggleSwitch { label: "I'd recommend it to a friend" } # asField @"recommend"
          textField { label: "Nickname" } # asField @"nickname"
          divider
      ) # mvu freshImpression
      p ( RecordToRecord.do
          staticText "Preview: "
          text # forField @"stars" starGlyphs
          text # forField @"headline" headlineQuote
          staticText " · owned "
          text # forField @"owned" ownedText
          text # projected recommendNote ) # tapped
      button { label: "Submit review" } # asCase @"submitted"
      toast # forCase @"submitted" submittedLine
