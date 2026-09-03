module LoanCalculatorBootstrap (loanCalculatorBootstrap) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import LoanCalculatorLogic (appliedLine, cityCarLoan, interestShare, monthlyText, rateLine, totalInterestLine)
import PUI (armed, forCase, mvu, required)
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
              badge { variant: "primary" } (text monthlyText) )
          listGroupItem (text rateLine)
          listGroupItem (text totalInterestLine) ) # shown
      ( div $ RecordToRecord.do
          staticText "Interest share of total repayment"
          progress @"Interest share" interestShare ) # shown
      button @"Apply for this loan" {} # armed
      toast # forCase @"Apply for this loan" appliedLine
