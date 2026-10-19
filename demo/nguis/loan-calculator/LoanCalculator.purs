module LoanCalculator (Loan, Purpose, Quantity, applyForLoan, cityCarLoan, interestShare, monthlyLine, rateLine, totalInterestLine) where

import Prelude (const, discard, negate, pure, (*), (+), (-), (/), (<>))

import Data.Number (pow)
import Data.Number.Format (fixed, toStringWith)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

type Loan =
  { "Amount" :: Quantity
  , "Applicant" :: String
  , "Payment protection insurance" :: Boolean
  , "Purpose" :: Purpose
  , "Term" :: Quantity
  }

type Quantity =
  { current :: Number
  , max :: Number
  , min :: Number
  , step :: [ continuous :: {}, discrete :: Number ]
  }

type Purpose = [ "Car" :: {}, "Holiday" :: {}, "Home improvement" :: {} ]

cityCarLoan :: Loan
cityCarLoan =
  { "Applicant": ""
  , "Amount": { current: 12000.0, min: smallestLoan, max: largestLoan, step: .discrete loanIncrement }
  , "Term": { current: 5.0, min: shortestTerm, max: longestTerm, step: .discrete termIncrement }
  , "Purpose": ."Car" {}
  , "Payment protection insurance": false
  }

rateLine :: Loan -> String
rateLine loan = "Interest rate " <> toStringWith (fixed 1) (annualRate loan) <> "% p.a."

monthlyLine :: Loan -> String
monthlyLine loan = "Monthly payment €" <> toStringWith (fixed 2) (monthlyPayment loan)

totalInterestLine :: Loan -> String
totalInterestLine loan = "Total interest €" <> toStringWith (fixed 2) (totalInterest loan)

applyForLoan :: Loan -> Aff [ "Application received" :: Loan ]
applyForLoan loan = do
  delay (Milliseconds applicationDelay.ms)
  pure (."Application received" loan)

applicationDelay :: { ms :: Number }
applicationDelay = { ms: 800.0 }

monthlyPayment :: Loan -> Number
monthlyPayment loan =
  let monthlyRate = annualRate loan / 100.0 / 12.0
      months = loan."Term".current * 12.0
  in loan."Amount".current * monthlyRate / (1.0 - pow (1.0 + monthlyRate) (-months))

totalInterest :: Loan -> Number
totalInterest loan = monthlyPayment loan * loan."Term".current * 12.0 - loan."Amount".current

interestShare :: Loan -> Number
interestShare loan = totalInterest loan / (monthlyPayment loan * loan."Term".current * 12.0)

annualRate :: Loan -> Number
annualRate loan = basePurposeRate loan."Purpose" - (if loan."Payment protection insurance" then insuranceDiscount else 0.0)

insuranceDiscount :: Number
insuranceDiscount = 0.3

basePurposeRate :: Purpose -> Number
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
