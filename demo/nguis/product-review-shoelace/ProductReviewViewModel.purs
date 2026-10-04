module ProductReviewViewModel (freshImpression, previewLine, submittedLine) where

import Prelude ((<>), (-))

import Data.Int (round)
import Data.Monoid (power)
import Data.String (trim)
import Data.Variant.Case (caseText)

freshImpression :: { "Headline" :: String, "How long have you owned it?" :: [ "1–12 months" :: {}, "less than a month" :: {}, "more than a year" :: {} ], "I\'d recommend it to a friend" :: Boolean, "Nickname" :: String, "Overall rating" :: { current :: Number, max :: Int }, "Your review" :: String }
freshImpression =
  { "Overall rating": { current: 0.0, max: maxStars }
  , "Headline": ""
  , "Your review": ""
  , "How long have you owned it?": ."less than a month" {}
  , "I'd recommend it to a friend": false
  , "Nickname": ""
  }

previewLine :: { "Headline" :: String, "How long have you owned it?" :: [ "1–12 months" :: {}, "less than a month" :: {}, "more than a year" :: {} ], "I\'d recommend it to a friend" :: Boolean, "Nickname" :: String, "Overall rating" :: { current :: Number, max :: Int }, "Your review" :: String } -> String
previewLine r =
  "Preview: " <> starGlyphs r."Overall rating" <> headlineQuote r."Headline" <> " · owned " <> caseText r."How long have you owned it?" <> recommendNote r."I'd recommend it to a friend"

submittedLine :: { "Headline" :: String, "How long have you owned it?" :: [ "1–12 months" :: {}, "less than a month" :: {}, "more than a year" :: {} ], "I\'d recommend it to a friend" :: Boolean, "Nickname" :: String, "Overall rating" :: { current :: Number, max :: Int }, "Your review" :: String } -> String
submittedLine r =
  "Thanks" <> forReviewer { "Nickname": r."Nickname" } <> "! Your " <> starGlyphs r."Overall rating" <> " review is in."

forReviewer :: forall r1. { "Nickname" :: String | r1 } -> String
forReviewer { "Nickname": nickname } = case trim nickname of
  "" -> ""
  name -> ", " <> name

recommendNote :: Boolean -> String
recommendNote recommend = if recommend then " · would recommend" else ""

headlineQuote :: String -> String
headlineQuote headline = case trim headline of
  "" -> ""
  quote -> " “" <> quote <> "”"

starGlyphs :: forall r1. { current :: Number, max :: Int | r1 } -> String
starGlyphs { current, max } = power "★" (round current) <> power "☆" (max - round current)

maxStars :: Int
maxStars = 5
