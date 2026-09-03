module ProductReviewLogic (freshImpression, previewLine, starsText, submittedLine) where

import Prelude ((<>), (-))

import Data.Int (round)
import Data.Monoid (power)
import Data.String (trim)
import Data.Variant.Case (caseText)

freshImpression :: { "Overall rating" :: { current :: Number, max :: Int }, "Headline" :: String, "Your review" :: String, "How long have you owned it?" :: [ "less than a month" :: {}, "1–12 months" :: {}, "more than a year" :: {} ], "I'd recommend it to a friend" :: Boolean, "Nickname" :: String }
freshImpression =
  { "Overall rating": { current: 0.0, max: maxStars }
  , "Headline": ""
  , "Your review": ""
  , "How long have you owned it?": ."less than a month" {}
  , "I'd recommend it to a friend": false
  , "Nickname": ""
  }

starsText :: { "Overall rating" :: { current :: Number, max :: Int } } -> String
starsText r = starGlyphs r."Overall rating"

previewLine :: { "Overall rating" :: { current :: Number, max :: Int }, "Headline" :: String, "How long have you owned it?" :: [ "less than a month" :: {}, "1–12 months" :: {}, "more than a year" :: {} ], "I'd recommend it to a friend" :: Boolean } -> String
previewLine r =
  "Preview: " <> starsText { "Overall rating": r."Overall rating" } <> headlineQuote r."Headline" <> " · owned " <> caseText r."How long have you owned it?" <> recommendNote r."I'd recommend it to a friend"

submittedLine :: { "Overall rating" :: { current :: Number, max :: Int }, "Nickname" :: String } -> String
submittedLine r =
  "Thanks" <> forReviewer { "Nickname": r."Nickname" } <> "! Your " <> starsText { "Overall rating": r."Overall rating" } <> " review is in."

forReviewer :: { "Nickname" :: String } -> String
forReviewer { "Nickname": nickname } = case trim nickname of
  "" -> ""
  name -> ", " <> name

recommendNote :: Boolean -> String
recommendNote recommend = if recommend then " · would recommend" else ""

headlineQuote :: String -> String
headlineQuote headline = case trim headline of
  "" -> ""
  quote -> " “" <> quote <> "”"

starGlyphs :: { current :: Number, max :: Int } -> String
starGlyphs { current, max } = power "★" (round current) <> power "☆" (max - round current)

maxStars :: Int
maxStars = 5
