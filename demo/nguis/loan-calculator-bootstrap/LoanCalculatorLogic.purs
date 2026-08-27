module LoanCalculatorLogic (appliedLine, cityCarLoan, interestShare, monthlyText, rateText, totalInterestText) where

import Prelude (negate, show, (*), (+), (-), (/), (<>))

import Data.Int (round)
import Data.Number (pow)
import Data.Number.Format (fixed, toStringWith)
import Data.String (trim)
import Data.Variant (match)

cityCarLoan :: { "Applicant" :: String, "Amount (€)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Term (years)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ], "Payment protection insurance" :: Boolean }
cityCarLoan =
  { "Applicant": ""
  , "Amount (€)": { current: 12000.0, min: smallestLoan, max: largestLoan, step: .discrete loanIncrement }
  , "Term (years)": { current: 5.0, min: shortestTerm, max: longestTerm, step: .discrete termIncrement }
  , "Purpose": ."Car" {}
  , "Payment protection insurance": false
  }

appliedLine :: { "Applicant" :: String, "Amount (€)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Term (years)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ], "Payment protection insurance" :: Boolean } -> String
appliedLine loan =
  "Application received" <> forApplicant { "Applicant": loan."Applicant" }
    <> ": €" <> toStringWith (fixed 0) loan."Amount (€)".current
    <> " over " <> show (round loan."Term (years)".current) <> " years, "
    <> monthlyText { "Amount (€)": loan."Amount (€)", "Term (years)": loan."Term (years)", "Purpose": loan."Purpose", "Payment protection insurance": loan."Payment protection insurance" } <> " monthly"

forApplicant :: { "Applicant" :: String } -> String
forApplicant { "Applicant": applicant } = case trim applicant of
  "" -> ""
  name -> ", " <> name

monthlyText :: { "Amount (€)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Term (years)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ], "Payment protection insurance" :: Boolean } -> String
monthlyText loan = "€" <> toStringWith (fixed 2) (monthlyPayment loan)

rateText :: { "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ], "Payment protection insurance" :: Boolean } -> String
rateText { "Purpose": purpose, "Payment protection insurance": insured } = toStringWith (fixed 1) (annualRate { "Purpose": purpose, "Payment protection insurance": insured }) <> "% p.a."

totalInterestText :: { "Amount (€)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Term (years)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ], "Payment protection insurance" :: Boolean } -> String
totalInterestText loan = "€" <> toStringWith (fixed 2) (totalInterest loan)

monthlyPayment :: { "Amount (€)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Term (years)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ], "Payment protection insurance" :: Boolean } -> Number
monthlyPayment { "Amount (€)": amount, "Term (years)": years, "Purpose": purpose, "Payment protection insurance": insured } =
  let monthlyRate = annualRate { "Purpose": purpose, "Payment protection insurance": insured } / 100.0 / 12.0
      months = years.current * 12.0
  in amount.current * monthlyRate / (1.0 - pow (1.0 + monthlyRate) (-months))

totalInterest :: { "Amount (€)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Term (years)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ], "Payment protection insurance" :: Boolean } -> Number
totalInterest loan = monthlyPayment loan * loan."Term (years)".current * 12.0 - loan."Amount (€)".current

interestShare :: { "Amount (€)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Term (years)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ], "Payment protection insurance" :: Boolean } -> Number
interestShare loan = totalInterest loan / (monthlyPayment loan * loan."Term (years)".current * 12.0)

annualRate :: { "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ], "Payment protection insurance" :: Boolean } -> Number
annualRate { "Purpose": purpose, "Payment protection insurance": insured } = basePurposeRate purpose + (if insured then -0.3 else 0.0)

basePurposeRate :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ] -> Number
basePurposeRate = match { "Car": \_ -> 7.4, "Home improvement": \_ -> 4.9, "Holiday": \_ -> 9.9 }

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
