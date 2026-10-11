module LoanCalculatorBootstrap (loanCalculatorBootstrap) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import LoanCalculatorViewModel (appliedLine, cityCarLoan, interestShare, monthlyLine, rateLine, totalInterestLine)
import PUI (armed, looped, with)
import PUI.Pipeline as Pipeline
import PUI.Web ((<+>), choice, staticText, text)
import PUI.Web.Bootstrap (body, button, card, listGroup, listGroupItem, progress, select, sliderLive, textField, toast, toggleSwitch)
import PUI.Web.HTML (div)

loanCalculatorBootstrap :: Effect Unit
loanCalculatorBootstrap =
  body $ Pipeline.do
    ( Pipeline.do
      textField @"Applicant" {}
      sliderLive @"Amount (€)" {}
      sliderLive @"Term (years)" {}
      select @"Purpose" {}
        (choice @"Car" <+> choice @"Home improvement" <+> choice @"Holiday")
      toggleSwitch @"Payment protection insurance" {}
    ) # looped
      @( "Applicant" :: String
       , "Amount (€)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Term (years)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Purpose" :: [ "Car" :: {}, "Home improvement" :: {}, "Holiday" :: {} ]
       , "Payment protection insurance" :: Boolean
       ) # with cityCarLoan
    card $ Pipeline.do
      ( listGroup $ RecordToRecord.do
        listGroupItem (text monthlyLine)
        listGroupItem (text rateLine)
        listGroupItem (text totalInterestLine) )
      ( div $ RecordToRecord.do
        staticText "Interest share of total repayment"
        progress interestShare )
    button @"Apply for this loan" {} # armed
    toast @"Apply for this loan" appliedLine
