module LoanCalculatorBootstrap (loanCalculatorBootstrap) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import LoanCalculatorLogic (appliedLine, cityCarLoan, interestShare, monthlyText, rateText, totalInterestText)
import PUI (forCase, mvu, projected, required)
import PUI.Web.Bootstrap (badge, button, card, listGroup, listGroupItem, progress, select, sliderLive, textField, toast, toggleSwitch)
import PUI.Web (choice)
import PUI.Web.HTML (shown, body, div, staticText, text)
import QualifiedDo.Category as Category

loanCalculatorBootstrap :: Effect Unit
loanCalculatorBootstrap =
  body $
    card $ Category.do
      ( Category.do
          textField @"Applicant" {}
          sliderLive @"Amount (€)" {}
          sliderLive @"Term (years)" {}
          select @"Purpose" {}
            [ choice @"Car", choice @"Home improvement", choice @"Holiday" ] # required
          toggleSwitch @"Payment protection insurance" {}
      ) # mvu cityCarLoan
      ( listGroup $ RecordToRecord.do
          listGroupItem ( RecordToRecord.do
              staticText "Monthly payment "
              badge { variant: "primary" } (text @"monthly" # projected monthlyText) )
          listGroupItem ( RecordToRecord.do
              staticText "Interest rate "
              text @"rate" # projected rateText )
          listGroupItem ( RecordToRecord.do
              staticText "Total interest "
              text @"totalInterest" # projected totalInterestText ) ) # shown
      ( ( div $ RecordToRecord.do
          staticText "Interest share of total repayment"
          progress @"interestShare" ) # projected interestShare ) # shown
      button @"Apply for this loan" {}
      toast # forCase @"Apply for this loan" appliedLine
