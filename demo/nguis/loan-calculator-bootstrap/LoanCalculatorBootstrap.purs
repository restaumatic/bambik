module LoanCalculatorBootstrap (loanCalculatorBootstrap) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import LoanCalculatorLogic (appliedLine, cityCarLoan, interestShare, monthlyText, rateText, totalInterestText)
import PUI (asCase, asField, forCase, mvu, projected, required, tapped)
import PUI.Web.Bootstrap (badge, button, card, listGroup, listGroupItem, progress, select, sliderLive, textField, toast, toggleSwitch)
import PUI.Web.HTML (body, div, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

loanCalculatorBootstrap :: Effect Unit
loanCalculatorBootstrap =
  body $
    card { caption: "Loan calculator" } $ Semigroupoid.do
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
              badge { variant: "primary" } (text # projected monthlyText) )
          listGroupItem ( RecordToRecord.do
              staticText "Interest rate "
              text # projected rateText )
          listGroupItem ( RecordToRecord.do
              staticText "Total interest "
              text # projected totalInterestText ) ) # tapped
      ( div $ RecordToRecord.do
          staticText "Interest share of total repayment"
          progress ) # projected interestShare # tapped
      button { label: "Apply for this loan" } # asCase @"applied"
      toast # forCase @"applied" appliedLine
