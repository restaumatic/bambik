module ProductReview (freshImpression, previewLine, submitReview) where

import Prelude (discard, pure, (<>), (-))

import Data.Int (round)
import Data.Monoid (power)
import Data.String (trim)
import Data.Variant.Case (caseText)
import Effect.Aff (Aff, Milliseconds(..), delay)

freshImpression
  :: { "Headline" :: String
     , "How long have you owned it?" :: [ "1–12 months" :: {}
                                        , "less than a month" :: {}
                                        , "more than a year" :: {}
                                        ]
     , "I\'d recommend it to a friend" :: Boolean
     , "Nickname" :: String
     , "Overall rating" :: { current :: Number, max :: Int }
     , "Your review" :: String
     }
freshImpression =
  { "Overall rating": { current: 0.0, max: maxStars }
  , "Headline": ""
  , "Your review": ""
  , "How long have you owned it?": ."less than a month" {}
  , "I'd recommend it to a friend": false
  , "Nickname": ""
  }

previewLine
  :: { "Headline" :: String
     , "How long have you owned it?" :: [ "1–12 months" :: {}
                                        , "less than a month" :: {}
                                        , "more than a year" :: {}
                                        ]
     , "I\'d recommend it to a friend" :: Boolean
     , "Nickname" :: String
     , "Overall rating" :: { current :: Number, max :: Int }
     , "Your review" :: String
     }
  -> String
previewLine r =
  "Preview: " <> starGlyphs r."Overall rating" <> headlineQuote r."Headline" <> " · owned " <> caseText r."How long have you owned it?" <> recommendNote r."I'd recommend it to a friend"

submitReview
  :: { "Headline" :: String
     , "How long have you owned it?" :: [ "1–12 months" :: {}
                                        , "less than a month" :: {}
                                        , "more than a year" :: {}
                                        ]
     , "I\'d recommend it to a friend" :: Boolean
     , "Nickname" :: String
     , "Overall rating" :: { current :: Number, max :: Int }
     , "Your review" :: String
     }
  -> Aff [ "Review submitted" :: { "Headline" :: String
                                 , "How long have you owned it?" :: [ "1–12 months" :: {}
                                                                    , "less than a month" :: {}
                                                                    , "more than a year" :: {}
                                                                    ]
                                 , "I\'d recommend it to a friend" :: Boolean
                                 , "Nickname" :: String
                                 , "Overall rating" :: { current :: Number, max :: Int }
                                 , "Your review" :: String
                                 }
         ]
submitReview review = do
  delay (Milliseconds submissionDelay.ms)
  pure (."Review submitted" review)

submissionDelay :: { ms :: Number }
submissionDelay = { ms: 800.0 }

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
