module LoanCalculatorBootstrap (loanCalculatorBootstrap) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import LoanCalculatorLogic (appliedLine, cityCarLoan, interestShare, monthlyText, rateText, totalInterestText)
import PUI (forCase, mvu, projected, required, tapped)
import PUI.Web.Bootstrap (badge, button, card, listGroup, listGroupItem, progress, select, sliderLive, textField, toast, toggleSwitch)
import PUI.Web (choices)
import Data.Tuple.Nested ((/\))
import Type.Proxy (Proxy(..))
import PUI.Web.HTML (body, div, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

loanCalculatorBootstrap :: Effect Unit
loanCalculatorBootstrap =
  body $
    card { caption: "Loan calculator" } $ Semigroupoid.do
      ( RecordToRecord.do
          textField @"Applicant" {}
          sliderLive @"Amount (€)" {}
          sliderLive @"Term (years)" {}
          select @"Purpose" {}
            (choices (Proxy @"Car" /\ Proxy @"Home improvement" /\ Proxy @"Holiday")) # required
          toggleSwitch @"Payment protection insurance" {}
      ) # mvu cityCarLoan
      listGroup ( RecordToRecord.do
          listGroupItem ( RecordToRecord.do
              staticText "Monthly payment "
              badge { variant: "primary" } (text @"monthly" # projected monthlyText) )
          listGroupItem ( RecordToRecord.do
              staticText "Interest rate "
              text @"rate" # projected rateText )
          listGroupItem ( RecordToRecord.do
              staticText "Total interest "
              text @"totalInterest" # projected totalInterestText ) ) # tapped
      ( div $ RecordToRecord.do
          staticText "Interest share of total repayment"
          progress @"interestShare" ) # projected interestShare # tapped
      button @"Apply for this loan" {}
      toast # forCase @"Apply for this loan" appliedLine
