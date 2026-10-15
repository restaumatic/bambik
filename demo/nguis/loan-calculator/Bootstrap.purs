module LoanCalculator.Bootstrap (loanCalculator) where

import Prelude (($), (#), identity, Unit)

import Effect (Effect)
import LoanCalculator (applyForLoan, cityCarLoan, interestShare, monthlyLine, rateLine, totalInterestLine)
import PUI (action, looped, with)
import PUI.Web ((<+>), choice, staticText, text)
import PUI.Web.Bootstrap (body, button, card, indeterminateLinearProgress, listGroup, listGroupItem, progress, select, sliderLive, textField, toast, toggleSwitch)
import PUI.Web.HTML (div)
import QualifiedDo.Semigroupoid as Semigroupoid

loanCalculator :: Effect Unit
loanCalculator =
  body $ Semigroupoid.do
    textField @"Applicant"
    sliderLive @"Amount (€)"
    sliderLive @"Term (years)"
    select @"Purpose"
      (choice @"Car" <+> choice @"Home improvement" <+> choice @"Holiday")
    toggleSwitch @"Payment protection insurance"
    card $ Semigroupoid.do
      listGroup $ Semigroupoid.do
        listGroupItem $ text monthlyLine
        listGroupItem $ text rateLine
        listGroupItem $ text totalInterestLine
      div $ Semigroupoid.do
        staticText "Interest share of total repayment"
        progress interestShare
    button @"Apply for this loan"
    indeterminateLinearProgress # action @"Apply for this loan" @( "Application received" :: _ ) applyForLoan
    toast @"Application received" identity
  # looped
    @( "Applicant" :: String
     , "Amount (€)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , "Term (years)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ]
     , "Payment protection insurance" :: Boolean
     ) # with cityCarLoan
