module ProductReviewLogic (freshImpression, headlineQuote, ownedText, recommendNote, starGlyphs, submittedLine) where

import Prelude ((-), (<>))

import Data.Int (round)
import Data.Monoid (power)
import Data.String (trim)
import Data.Variant (match)

freshImpression :: { "Overall rating" :: { current :: Number, max :: Int }, "Headline" :: String, "Your review" :: String, "How long have you owned it?" :: [ underMonth :: {}, underYear :: {}, overYear :: {} ], "I'd recommend it to a friend" :: Boolean, "Nickname" :: String }
freshImpression =
  { "Overall rating": { current: 0.0, max: maxStars }
  , "Headline": ""
  , "Your review": ""
  , "How long have you owned it?": .underMonth {}
  , "I'd recommend it to a friend": false
  , "Nickname": ""
  }

submittedLine :: { "Overall rating" :: { current :: Number, max :: Int }, "Headline" :: String, "Your review" :: String, "How long have you owned it?" :: [ underMonth :: {}, underYear :: {}, overYear :: {} ], "I'd recommend it to a friend" :: Boolean, "Nickname" :: String } -> String
submittedLine { "Overall rating": stars, "Nickname": nickname } =
  "Thanks" <> forReviewer { "Nickname": nickname } <> "! Your " <> starGlyphs stars <> " review is in."

forReviewer :: { "Nickname" :: String } -> String
forReviewer { "Nickname": nickname } = case trim nickname of
  "" -> ""
  name -> ", " <> name

recommendNote :: { "I'd recommend it to a friend" :: Boolean } -> String
recommendNote { "I'd recommend it to a friend": recommend } = if recommend then " · would recommend" else ""

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
