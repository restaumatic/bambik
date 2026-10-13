module LoanCalculatorBootstrap (loanCalculatorBootstrap) where

import Prelude (Unit, ($), (#))

import Effect (Effect)
import LoanCalculatorViewModel (appliedLine, cityCarLoan, interestShare, monthlyLine, rateLine, totalInterestLine)
import PUI (looped, with)
import PUI.Web ((<+>), choice, staticText, text)
import PUI.Web.Bootstrap (body, button, card, listGroup, listGroupItem, progress, select, sliderLive, textField, toast, toggleSwitch)
import PUI.Web.HTML (div)
import QualifiedDo.Semigroupoid as Semigroupoid

loanCalculatorBootstrap :: Effect Unit
loanCalculatorBootstrap =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      textField @"Applicant"
      sliderLive @"Amount (€)"
      sliderLive @"Term (years)"
      select @"Purpose"
        (choice @"Car" <+> choice @"Home improvement" <+> choice @"Holiday")
      toggleSwitch @"Payment protection insurance"
    ) # looped
      @( "Applicant" :: String
       , "Amount (€)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Term (years)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ]
       , "Payment protection insurance" :: Boolean
       ) # with cityCarLoan
    card $ Semigroupoid.do
      listGroup $ Semigroupoid.do
        listGroupItem (text monthlyLine)
        listGroupItem (text rateLine)
        listGroupItem (text totalInterestLine)
      div $ Semigroupoid.do
        staticText "Interest share of total repayment"
        progress interestShare
    button @"Apply for this loan"
    toast @"Apply for this loan" appliedLine
