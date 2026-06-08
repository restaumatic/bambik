module Showcase.Logic where

import Data.Profunctor.Row.Example (MyRowToRowProfunctor, actionButton, checkbox, eventLog, modal, notification, request, statusBar, submit, textInput)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (Variant)
import QualifiedDo.Semigroupoid as Semigroupoid

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
             (Variant ( submit :: { email :: String, cardNumber :: String, savePayment :: Boolean } ))
             (Variant ( thankYou :: String, failure :: String )) )
    ( notification
        :: MyRowToRowProfunctor
             (Variant ( editing :: String ))
             (Variant ( editing :: String )) )
    ( modal
        :: MyRowToRowProfunctor
             (Variant ( cancel :: {} ))
             (Variant ( cancelled :: String )) )
  VariantToRecord.do
    statusBar @"thankYou"
    eventLog @"failure"
    statusBar @"editing"
    statusBar @"cancelled"
