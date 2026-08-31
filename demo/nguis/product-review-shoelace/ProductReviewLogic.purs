module ProductReviewLogic (freshImpression, presentReview, submittedLine) where

import Prelude ((<>), (-))

import Data.Int (round)
import Data.Monoid (power)
import Data.String (trim)
import Data.Variant.Case (caseText)

freshImpression :: { "Overall rating" :: { current :: Number, max :: Int }, "Headline" :: String, "Your review" :: String, "How long have you owned it?" :: [ "less than a month" :: {}, "1–12 months" :: {}, "more than a year" :: {} ], "I'd recommend it to a friend" :: Boolean, "Nickname" :: String, starsText :: String, quoteText :: String, ownedText :: String, recommendText :: String }
freshImpression = presentReview
  { "Overall rating": { current: 0.0, max: maxStars }
  , "Headline": ""
  , "Your review": ""
  , "How long have you owned it?": ."less than a month" {}
  , "I'd recommend it to a friend": false
  , "Nickname": ""
  , starsText: ""
  , quoteText: ""
  , ownedText: ""
  , recommendText: ""
  }

presentReview :: { "Overall rating" :: { current :: Number, max :: Int }, "Headline" :: String, "Your review" :: String, "How long have you owned it?" :: [ "less than a month" :: {}, "1–12 months" :: {}, "more than a year" :: {} ], "I'd recommend it to a friend" :: Boolean, "Nickname" :: String, starsText :: String, quoteText :: String, ownedText :: String, recommendText :: String } -> { "Overall rating" :: { current :: Number, max :: Int }, "Headline" :: String, "Your review" :: String, "How long have you owned it?" :: [ "less than a month" :: {}, "1–12 months" :: {}, "more than a year" :: {} ], "I'd recommend it to a friend" :: Boolean, "Nickname" :: String, starsText :: String, quoteText :: String, ownedText :: String, recommendText :: String }
presentReview r = r
  { starsText = starGlyphs r."Overall rating"
  , quoteText = headlineQuote r."Headline"
  , ownedText = caseText r."How long have you owned it?"
  , recommendText = recommendNote r."I'd recommend it to a friend"
  }

submittedLine :: { starsText :: String, "Nickname" :: String } -> String
submittedLine { starsText, "Nickname": nickname } =
  "Thanks" <> forReviewer { "Nickname": nickname } <> "! Your " <> starsText <> " review is in."

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
