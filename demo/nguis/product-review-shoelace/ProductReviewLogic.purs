module ProductReviewLogic (freshImpression, headlineQuote, previewLine, recommendNote, starGlyphs, submittedLine) where

import Data.Variant.Case (caseText)
import Prelude ((<>), (-))

import Data.Int (round)
import Data.Monoid (power)
import Data.String (trim)

freshImpression :: { "Overall rating" :: { current :: Number, max :: Int }, "Headline" :: String, "Your review" :: String, "How long have you owned it?" :: [ "less than a month" :: {}, "1–12 months" :: {}, "more than a year" :: {} ], "I'd recommend it to a friend" :: Boolean, "Nickname" :: String }
freshImpression =
  { "Overall rating": { current: 0.0, max: maxStars }
  , "Headline": ""
  , "Your review": ""
  , "How long have you owned it?": ."less than a month" {}
  , "I'd recommend it to a friend": false
  , "Nickname": ""
  }

submittedLine :: { "Overall rating" :: { current :: Number, max :: Int }, "Headline" :: String, "Your review" :: String, "How long have you owned it?" :: [ "less than a month" :: {}, "1–12 months" :: {}, "more than a year" :: {} ], "I'd recommend it to a friend" :: Boolean, "Nickname" :: String } -> String
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

maxStars :: Int
maxStars = 5

previewLine :: { "Overall rating" :: { current :: Number, max :: Int }, "Headline" :: String, "How long have you owned it?" :: [ "less than a month" :: {}, "1–12 months" :: {}, "more than a year" :: {} ], "I'd recommend it to a friend" :: Boolean } -> String
previewLine { "Overall rating": rating, "Headline": headline, "How long have you owned it?": owned, "I'd recommend it to a friend": recommend } =
  "Preview: " <> starGlyphs rating <> headlineQuote headline <> " \x00b7 owned " <> caseText owned <> recommendNote { "I'd recommend it to a friend": recommend }
