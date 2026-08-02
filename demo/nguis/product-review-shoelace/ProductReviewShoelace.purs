module ProductReviewShoelace (productReviewShoelace) where

import Prelude (Unit, ($), (#), (-), (<>))

import Data.Int (round)
import Data.Monoid (power)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (trim)
import Data.Variant (match)
import Effect (Effect)
import PUI (PUI, asCase, asField, forCase, forField, mvu, projected, required, tapped)
import PUI.Web.HTML (body, p, staticText, text)
import PUI.Web.Shoelace (button, card, divider, rating, select, textArea, textField, toast, toggleSwitch)
import PUI.Web (Web)
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
      submittedToast

freshImpression :: { stars :: { current :: Number, max :: Int }, headline :: String, review :: String, owned :: [ underMonth :: {}, underYear :: {}, overYear :: {} ], recommend :: Boolean, nickname :: String }
freshImpression =
  { stars: { current: 0.0, max: maxStars }
  , headline: ""
  , review: ""
  , owned: .underMonth {}
  , recommend: false
  , nickname: ""
  }

submittedToast :: PUI Web [ submitted :: { stars :: { current :: Number, max :: Int }, headline :: String, review :: String, owned :: [ underMonth :: {}, underYear :: {}, overYear :: {} ], recommend :: Boolean, nickname :: String } ] {}
submittedToast = toast # forCase @"submitted" submittedLine

submittedLine :: { stars :: { current :: Number, max :: Int }, headline :: String, review :: String, owned :: [ underMonth :: {}, underYear :: {}, overYear :: {} ], recommend :: Boolean, nickname :: String } -> String
submittedLine { stars, nickname } =
  "Thanks" <> forReviewer { nickname } <> "! Your " <> starGlyphs stars <> " review is in."

forReviewer :: { nickname :: String } -> String
forReviewer { nickname } = case trim nickname of
  "" -> ""
  name -> ", " <> name

recommendNote :: { recommend :: Boolean } -> String
recommendNote { recommend } = if recommend then " · would recommend" else ""

headlineQuote :: String -> String
headlineQuote headline = case trim headline of
  "" -> ""
  quote -> " “" <> quote <> "”"

starGlyphs :: { current :: Number, max :: Int } -> String
starGlyphs { current, max } = power "★" (round current) <> power "☆" (max - round current)

ownedText :: [ underMonth :: {}, underYear :: {}, overYear :: {} ] -> String
ownedText = match { underMonth: \_ -> "less than a month", underYear: \_ -> "1–12 months", overYear: \_ -> "more than a year" }

maxStars :: Int
maxStars = 5
