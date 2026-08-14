module LoanCalculatorBootstrap (loanCalculatorBootstrap) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import LoanCalculatorLogic (appliedLine, cityCarLoan, interestShare, monthlyText, rateText, totalInterestText)
import PUI (asCase, forCase, mvu, projected, required, tapped)
import PUI.Web.Bootstrap (badge, button, card, listGroup, listGroupItem, progress, select, sliderLive, textField, toast, toggleSwitch)
import PUI.Web.HTML (body, div, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

loanCalculatorBootstrap :: Effect Unit
loanCalculatorBootstrap =
  body $
    card { caption: "Loan calculator" } $ Semigroupoid.do
      ( RecordToRecord.do
          textField @"applicant" { label: "Applicant" }
          sliderLive @"amount" { label: "Amount (€)" }
          sliderLive @"years" { label: "Term (years)" }
          select @"purpose" { label: "Purpose" }
            [ { value: .car {}, label: "Car" }
            , { value: .home {}, label: "Home improvement" }
            , { value: .holiday {}, label: "Holiday" }
            ] # required
          toggleSwitch @"insured" { label: "Payment protection insurance" }
      ) # mvu cityCarLoan
      listGroup ( RecordToRecord.do
          listGroupItem ( RecordToRecord.do
              staticText "Monthly payment "
              badge { variant: "primary" } (text @"value" # projected @"value" monthlyText) )
          listGroupItem ( RecordToRecord.do
              staticText "Interest rate "
              text @"value" # projected @"value" rateText )
          listGroupItem ( RecordToRecord.do
              staticText "Total interest "
              text @"value" # projected @"value" totalInterestText ) ) # tapped
      ( div $ RecordToRecord.do
          staticText "Interest share of total repayment"
          progress @"value" ) # projected @"value" interestShare # tapped
      button { label: "Apply for this loan" } # asCase @"clicked" @"applied"
      toast # forCase @"event" @"applied" appliedLine
