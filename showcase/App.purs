module Showcase.App where

import PUI.Data.Profunctor.Row.Example (MyRowToRowProfunctor, actionButton, checkbox, eventLog, modal, notification, request, statusBar, submit, textInput)
import PUI.Data.Profunctor.Row.RecordToRecord as RecordToRecord
import PUI.Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI.Data.Profunctor.Row.VariantToRecord as VariantToRecord
import PUI.Data.Profunctor.Row.VariantToVariant as VariantToVariant
import QualifiedDo.Semigroupoid as Semigroupoid

checkout ∷ MyRowToRowProfunctor 
  { cardNumber ∷ String , email ∷ String , savePayment ∷ Boolean } 
  { cancelled ∷ String , editing ∷ String , failure ∷ String , thankYou ∷ String }
checkout = Semigroupoid.do
  RecordToRecord.do
    textInput @"email"
    textInput @"cardNumber"
    checkbox @"savePayment"
  RecordToVariant.do
    submit @"submit" @"editing"
    actionButton @"cancel"
  VariantToVariant.do
    ( request
        :: MyRowToRowProfunctor
             [ submit :: { email :: String, cardNumber :: String, savePayment :: Boolean } ]
             [ thankYou :: String, failure :: String ] )
    ( notification
        :: MyRowToRowProfunctor
             [ editing :: String ]
             [ editing :: String ] )
    ( modal
        :: MyRowToRowProfunctor
             [ cancel :: {} ]
             [ cancelled :: String ] )
  VariantToRecord.do
    statusBar @"thankYou"
    eventLog @"failure"
    statusBar @"editing"
    statusBar @"cancelled"
