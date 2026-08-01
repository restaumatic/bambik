module LoanCalculatorBootstrap (loanCalculatorBootstrap) where

import Prelude (Unit, negate, show, ($), (#), (*), (+), (-), (/), (<>))

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Number (pow)
import Data.Number.Format (fixed, toStringWith)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (trim)
import Data.Variant (match)
import Effect (Effect)
import PUI (PUI, asCase, asField, forCase, mvu, projection, required, tapped)
import PUI.Bootstrap (badge, button, card, listGroup, listGroupItem, progress, select, sliderLive, textField, toast, toggleSwitch)
import PUI.HTML (body, div, staticText, text)
import PUI.Web (Web)
import QualifiedDo.Semigroupoid as Semigroupoid

loanCalculatorBootstrap :: Effect Unit
loanCalculatorBootstrap =
  body $
    card { caption: "Loan calculator" } Semigroupoid.do
      ( RecordToRecord.do
          textField { label: "Applicant" } # asField @"applicant"
          sliderLive { label: "Amount (€)" } # asField @"amount"
          sliderLive { label: "Term (years)" } # asField @"years"
          select { label: "Purpose" }
            [ { value: .car {}, label: "Car" }
            , { value: .home {}, label: "Home improvement" }
            , { value: .holiday {}, label: "Holiday" }
            ] # required # asField @"purpose"
          toggleSwitch { label: "Payment protection insurance" } # asField @"insured"
      ) # mvu cityCarLoan
      listGroup ( RecordToRecord.do
          listGroupItem ( RecordToRecord.do
              staticText "Monthly payment "
              badge { variant: "primary" } (text # projection monthlyText) )
          listGroupItem ( RecordToRecord.do
              staticText "Interest rate "
              text # projection rateText )
          listGroupItem ( RecordToRecord.do
              staticText "Total interest "
              text # projection totalInterestText )
      ) # tapped
      ( div $ RecordToRecord.do
          staticText "Interest share of total repayment"
          progress ) # projection interestShare # tapped
      button { label: "Apply for this loan" } # asCase @"applied"
      appliedToast

cityCarLoan :: { applicant :: String, amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, purpose :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean }
cityCarLoan =
  { applicant: ""
  , amount: { current: 12000.0, min: smallestLoan, max: largestLoan, step: Just loanIncrement }
  , years: { current: 5.0, min: shortestTerm, max: longestTerm, step: Just termIncrement }
  , purpose: .car {}
  , insured: false
  }

appliedToast :: PUI Web [ applied :: { applicant :: String, amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, purpose :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } ] {}
appliedToast = toast # forCase @"applied" # lcmap (match { applied: \loan -> .applied (appliedLine loan) })

appliedLine :: { applicant :: String, amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, purpose :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> String
appliedLine loan =
  "Application received" <> forApplicant { applicant: loan.applicant }
    <> ": €" <> toStringWith (fixed 0) loan.amount.current
    <> " over " <> show (round loan.years.current) <> " years, "
    <> monthlyText { amount: loan.amount, years: loan.years, purpose: loan.purpose, insured: loan.insured } <> " monthly"

forApplicant :: { applicant :: String } -> String
forApplicant { applicant } = case trim applicant of
  "" -> ""
  name -> ", " <> name

monthlyText :: { amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, purpose :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> String
monthlyText loan = "€" <> toStringWith (fixed 2) (monthlyPayment loan)

rateText :: { purpose :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> String
rateText { purpose, insured } = toStringWith (fixed 1) (annualRate { purpose, insured }) <> "% p.a."

totalInterestText :: { amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, purpose :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> String
totalInterestText loan = "€" <> toStringWith (fixed 2) (totalInterest loan)

monthlyPayment :: { amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, purpose :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> Number
monthlyPayment { amount, years, purpose, insured } =
  let monthlyRate = annualRate { purpose, insured } / 100.0 / 12.0
      months = years.current * 12.0
  in amount.current * monthlyRate / (1.0 - pow (1.0 + monthlyRate) (-months))

totalInterest :: { amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, purpose :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> Number
totalInterest loan = monthlyPayment loan * loan.years.current * 12.0 - loan.amount.current

interestShare :: { amount :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, years :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, purpose :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> Number
interestShare loan = totalInterest loan / (monthlyPayment loan * loan.years.current * 12.0)

annualRate :: { purpose :: [ car :: {}, home :: {}, holiday :: {} ], insured :: Boolean } -> Number
annualRate { purpose, insured } = basePurposeRate purpose + (if insured then -0.3 else 0.0)

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
