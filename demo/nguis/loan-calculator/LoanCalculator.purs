module LoanCalculator (applyForLoan, cityCarLoan, interestShare, monthlyLine, rateLine, totalInterestLine) where

import Prelude (const, discard, negate, pure, (*), (+), (-), (/), (<>))

import Data.Number (pow)
import Data.Number.Format (fixed, toStringWith)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

cityCarLoan
  :: { "Amount (€)" :: { current :: Number
                       , max :: Number
                       , min :: Number
                       , step :: [ continuous :: {}, discrete :: Number ]
                       }
     , "Applicant" :: String
     , "Payment protection insurance" :: Boolean
     , "Purpose" :: [ "Car" :: {}, "Holiday" :: {}, "Home improvement" :: {} ]
     , "Term (years)" :: { current :: Number
                         , max :: Number
                         , min :: Number
                         , step :: [ continuous :: {}, discrete :: Number ]
                         }
     }
cityCarLoan =
  { "Applicant": ""
  , "Amount (€)": { current: 12000.0, min: smallestLoan, max: largestLoan, step: .discrete loanIncrement }
  , "Term (years)": { current: 5.0, min: shortestTerm, max: longestTerm, step: .discrete termIncrement }
  , "Purpose": ."Car" {}
  , "Payment protection insurance": false
  }

rateLine
  :: { "Amount (€)" :: { current :: Number
                       , max :: Number
                       , min :: Number
                       , step :: [ continuous :: {}, discrete :: Number ]
                       }
     , "Applicant" :: String
     , "Payment protection insurance" :: Boolean
     , "Purpose" :: [ "Car" :: {}, "Holiday" :: {}, "Home improvement" :: {} ]
     , "Term (years)" :: { current :: Number
                         , max :: Number
                         , min :: Number
                         , step :: [ continuous :: {}, discrete :: Number ]
                         }
     }
  -> String
rateLine r = "Interest rate " <> toStringWith (fixed 1) (annualRate r) <> "% p.a."

monthlyLine
  :: { "Amount (€)" :: { current :: Number
                       , max :: Number
                       , min :: Number
                       , step :: [ continuous :: {}, discrete :: Number ]
                       }
     , "Applicant" :: String
     , "Payment protection insurance" :: Boolean
     , "Purpose" :: [ "Car" :: {}, "Holiday" :: {}, "Home improvement" :: {} ]
     , "Term (years)" :: { current :: Number
                         , max :: Number
                         , min :: Number
                         , step :: [ continuous :: {}, discrete :: Number ]
                         }
     }
  -> String
monthlyLine loan = "Monthly payment " <> monthlyText loan

totalInterestLine
  :: { "Amount (€)" :: { current :: Number
                       , max :: Number
                       , min :: Number
                       , step :: [ continuous :: {}, discrete :: Number ]
                       }
     , "Applicant" :: String
     , "Payment protection insurance" :: Boolean
     , "Purpose" :: [ "Car" :: {}, "Holiday" :: {}, "Home improvement" :: {} ]
     , "Term (years)" :: { current :: Number
                         , max :: Number
                         , min :: Number
                         , step :: [ continuous :: {}, discrete :: Number ]
                         }
     }
  -> String
totalInterestLine loan = "Total interest €" <> toStringWith (fixed 2) (totalInterest loan)

applyForLoan
  :: { "Amount (€)" :: { current :: Number
                       , max :: Number
                       , min :: Number
                       , step :: [ continuous :: {}, discrete :: Number ]
                       }
     , "Applicant" :: String
     , "Payment protection insurance" :: Boolean
     , "Purpose" :: [ "Car" :: {}, "Holiday" :: {}, "Home improvement" :: {} ]
     , "Term (years)" :: { current :: Number
                         , max :: Number
                         , min :: Number
                         , step :: [ continuous :: {}, discrete :: Number ]
                         }
     }
  -> Aff [ "Application received" :: { "Amount (€)" :: { current :: Number
                                                        , max :: Number
                                                        , min :: Number
                                                        , step :: [ continuous :: {}, discrete :: Number ]
                                                        }
                                      , "Applicant" :: String
                                      , "Payment protection insurance" :: Boolean
                                      , "Purpose" :: [ "Car" :: {}, "Holiday" :: {}, "Home improvement" :: {} ]
                                      , "Term (years)" :: { current :: Number
                                                          , max :: Number
                                                          , min :: Number
                                                          , step :: [ continuous :: {}, discrete :: Number ]
                                                          }
                                      }
         ]
applyForLoan loan = do
  delay (Milliseconds applicationDelay.ms)
  pure (."Application received" loan)

applicationDelay :: { ms :: Number }
applicationDelay = { ms: 800.0 }

monthlyText :: forall r1. { "Amount (€)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Term (years)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ], "Payment protection insurance" :: Boolean | r1 } -> String
monthlyText loan = "€" <> toStringWith (fixed 2) (monthlyPayment loan)

monthlyPayment :: forall r1. { "Amount (€)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Term (years)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ], "Payment protection insurance" :: Boolean | r1 } -> Number
monthlyPayment { "Amount (€)": amount, "Term (years)": years, "Purpose": purpose, "Payment protection insurance": insured } =
  let monthlyRate = annualRate { "Purpose": purpose, "Payment protection insurance": insured } / 100.0 / 12.0
      months = years.current * 12.0
  in amount.current * monthlyRate / (1.0 - pow (1.0 + monthlyRate) (-months))

totalInterest :: forall r1. { "Amount (€)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Term (years)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ], "Payment protection insurance" :: Boolean | r1 } -> Number
totalInterest loan = monthlyPayment loan * loan."Term (years)".current * 12.0 - loan."Amount (€)".current

interestShare
  :: { "Amount (€)" :: { current :: Number
                       , max :: Number
                       , min :: Number
                       , step :: [ continuous :: {}, discrete :: Number ]
                       }
     , "Applicant" :: String
     , "Payment protection insurance" :: Boolean
     , "Purpose" :: [ "Car" :: {}, "Holiday" :: {}, "Home improvement" :: {} ]
     , "Term (years)" :: { current :: Number
                         , max :: Number
                         , min :: Number
                         , step :: [ continuous :: {}, discrete :: Number ]
                         }
     }
  -> Number
interestShare loan = totalInterest loan / (monthlyPayment loan * loan."Term (years)".current * 12.0)

annualRate :: forall r1. { "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ], "Payment protection insurance" :: Boolean | r1 } -> Number
annualRate { "Purpose": purpose, "Payment protection insurance": insured } = basePurposeRate purpose + (if insured then -0.3 else 0.0)

basePurposeRate :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ] -> Number
basePurposeRate = match { "Car": const 7.4, "Home improvement": const 4.9, "Holiday": const 9.9 }

smallestLoan :: Number
smallestLoan = 1000.0

largestLoan :: Number
largestLoan = 50000.0

loanIncrement :: Number
loanIncrement = 500.0

shortestTerm :: Number
shortestTerm = 1.0

longestTerm :: Number
longestTerm = 10.0

termIncrement :: Number
termIncrement = 1.0
