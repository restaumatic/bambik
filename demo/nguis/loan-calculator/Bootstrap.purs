module LoanCalculator.Bootstrap (loanCalculator) where

import Prelude (($), (#), Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import LoanCalculator (applyForLoan, cityCarLoan, interestShare, monthlyLine, rateLine, totalInterestLine)
import PUI (action, kept, looped, stepped, with)
import PUI.Web (staticText, text)
import PUI.Web.Bootstrap (body, button, card, indeterminateLinearProgress, listGroup, listGroupItem, option, progress, select, sliderLive', textField, toast, toggleSwitch)
import PUI.Web.HTML (div)
import QualifiedDo.Semigroupoid as Semigroupoid

loanCalculator :: Effect Unit
loanCalculator =
  body $ Semigroupoid.do
    textField @"Applicant"
    sliderLive' @"Amount" { unit: "€" }
    sliderLive' @"Term" { unit: "years" }
    select @"Purpose" $ VariantToVariant.do
      option @"Car"
      option @"Home improvement"
      option @"Holiday"
    toggleSwitch @"Payment protection insurance"
    card $ Semigroupoid.do
      listGroup $ Semigroupoid.do
        listGroupItem $ text monthlyLine
        listGroupItem $ text rateLine
        listGroupItem $ text totalInterestLine
      div $ Semigroupoid.do
        staticText "Interest share of total repayment"
        progress interestShare
    ( Semigroupoid.do
      button @"Apply for this loan"
      indeterminateLinearProgress # action @"Apply for this loan" applyForLoan
      toast @"Application received" kept ) # stepped
  # looped # with cityCarLoan
