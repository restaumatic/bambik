module ProductReview (freshImpression, previewLine, submitReview) where

import Prelude (discard, pure, (<>), (-))

import Data.Int (round)
import Data.Monoid (power)
import Data.String (toLower, trim)
import Data.Variant.Case (caseText)
import Effect.Aff (Aff, Milliseconds(..), delay)

freshImpression
  :: { "Headline" :: String
     , "Owned for" :: [ "Up to a year" :: {}
                                        , "Under a month" :: {}
                                        , "Over a year" :: {}
                                        ]
     , "Recommend to a friend" :: Boolean
     , "Nickname" :: String
     , "Overall rating" :: { current :: Number, max :: Int }
     , "Review" :: String
     }
freshImpression =
  { "Overall rating": { current: 0.0, max: maxStars }
  , "Headline": ""
  , "Review": ""
  , "Owned for": ."Under a month" {}
  , "Recommend to a friend": false
  , "Nickname": ""
  }

previewLine
  :: { "Headline" :: String
     , "Owned for" :: [ "Up to a year" :: {}
                                        , "Under a month" :: {}
                                        , "Over a year" :: {}
                                        ]
     , "Recommend to a friend" :: Boolean
     , "Nickname" :: String
     , "Overall rating" :: { current :: Number, max :: Int }
     , "Review" :: String
     }
  -> String
previewLine r =
  "Preview: " <> starGlyphs r."Overall rating" <> headlineQuote r."Headline" <> " · owned for " <> toLower (caseText r."Owned for") <> recommendNote r."Recommend to a friend"

submitReview
  :: { "Headline" :: String
     , "Owned for" :: [ "Up to a year" :: {}
                                        , "Under a month" :: {}
                                        , "Over a year" :: {}
                                        ]
     , "Recommend to a friend" :: Boolean
     , "Nickname" :: String
     , "Overall rating" :: { current :: Number, max :: Int }
     , "Review" :: String
     }
  -> Aff [ "Review submitted" :: { "Headline" :: String
                                 , "Owned for" :: [ "Up to a year" :: {}
                                                                    , "Under a month" :: {}
                                                                    , "Over a year" :: {}
                                                                    ]
                                 , "Recommend to a friend" :: Boolean
                                 , "Nickname" :: String
                                 , "Overall rating" :: { current :: Number, max :: Int }
                                 , "Review" :: String
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
