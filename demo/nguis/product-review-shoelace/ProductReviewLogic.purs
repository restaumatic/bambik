module ProductReviewLogic (freshImpression, headlineQuote, ownedText, recommendNote, starGlyphs, submittedLine) where

import Prelude ((-), (<>))

import Data.Int (round)
import Data.Monoid (power)
import Data.String (trim)
import Data.Variant (match)

freshImpression :: { stars :: { current :: Number, max :: Int }, "Headline" :: String, review :: String, owned :: [ underMonth :: {}, underYear :: {}, overYear :: {} ], recommend :: Boolean, "Nickname" :: String }
freshImpression =
  { stars: { current: 0.0, max: maxStars }
  , "Headline": ""
  , review: ""
  , owned: .underMonth {}
  , recommend: false
  , "Nickname": ""
  }

submittedLine :: { stars :: { current :: Number, max :: Int }, "Headline" :: String, review :: String, owned :: [ underMonth :: {}, underYear :: {}, overYear :: {} ], recommend :: Boolean, "Nickname" :: String } -> String
submittedLine { stars, "Nickname": nickname } =
  "Thanks" <> forReviewer { "Nickname": nickname } <> "! Your " <> starGlyphs stars <> " review is in."

forReviewer :: { "Nickname" :: String } -> String
forReviewer { "Nickname": nickname } = case trim nickname of
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
