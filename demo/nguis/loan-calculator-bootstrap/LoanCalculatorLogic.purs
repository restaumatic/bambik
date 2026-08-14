module LoanCalculatorLogic (appliedLine, cityCarLoan, interestShare, monthlyText, rateText, totalInterestText) where

import Prelude (negate, show, (*), (+), (-), (/), (<>))

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Number (pow)
import Data.Number.Format (fixed, toStringWith)
import Data.String (trim)
import Data.Variant (match)

cityCarLoan :: { "Applicant" :: String, amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Purpose" :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean }
cityCarLoan =
  { "Applicant": ""
  , amount: { current: 12000.0, min: smallestLoan, max: largestLoan, step: Just loanIncrement }
  , years: { current: 5.0, min: shortestTerm, max: longestTerm, step: Just termIncrement }
  , "Purpose": .car {}
  , insured: false
  }

appliedLine :: { "Applicant" :: String, amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Purpose" :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> String
appliedLine loan =
  "Application received" <> forApplicant { "Applicant": loan."Applicant" }
    <> ": €" <> toStringWith (fixed 0) loan.amount.current
    <> " over " <> show (round loan.years.current) <> " years, "
    <> monthlyText { amount: loan.amount, years: loan.years, "Purpose": loan."Purpose", insured: loan.insured } <> " monthly"

forApplicant :: { "Applicant" :: String } -> String
forApplicant { "Applicant": applicant } = case trim applicant of
  "" -> ""
  name -> ", " <> name

monthlyText :: { amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Purpose" :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> String
monthlyText loan = "€" <> toStringWith (fixed 2) (monthlyPayment loan)

rateText :: { "Purpose" :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> String
rateText { "Purpose": purpose, insured } = toStringWith (fixed 1) (annualRate { "Purpose": purpose, insured }) <> "% p.a."

totalInterestText :: { amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Purpose" :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> String
totalInterestText loan = "€" <> toStringWith (fixed 2) (totalInterest loan)

monthlyPayment :: { amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Purpose" :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> Number
monthlyPayment { amount, years, "Purpose": purpose, insured } =
  let monthlyRate = annualRate { "Purpose": purpose, insured } / 100.0 / 12.0
      months = years.current * 12.0
  in amount.current * monthlyRate / (1.0 - pow (1.0 + monthlyRate) (-months))

totalInterest :: { amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Purpose" :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> Number
totalInterest loan = monthlyPayment loan * loan.years.current * 12.0 - loan.amount.current

interestShare :: { amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Purpose" :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> Number
interestShare loan = totalInterest loan / (monthlyPayment loan * loan.years.current * 12.0)

annualRate :: { "Purpose" :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> Number
annualRate { "Purpose": purpose, insured } = basePurposeRate purpose + (if insured then -0.3 else 0.0)

basePurposeRate :: [ car :: {}, home :: {}, holiday :: {} ] -> Number
basePurposeRate = match { car: \_ -> 7.4, home: \_ -> 4.9, holiday: \_ -> 9.9 }

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
