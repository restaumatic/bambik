module Showcase.Logic where

import Data.Profunctor.Row.Example (MyRowToRowProfunctor, actionButton, button, checkbox, eventLog, modal, request, statusBar, textInput)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Either (Either)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import QualifiedDo.Semigroupoid as Semigroupoid

checkout = Semigroupoid.do
  RecordToRecord.do
    textInput @"email"
    textInput @"cardNumber"
    checkbox @"savePayment"
  RecordToVariant.do
    button @"submit"
    actionButton @"cancel"
  VariantToVariant.do
    ( request
        :: MyRowToRowProfunctor
             (Variant ( submit :: Record ( email :: String, cardNumber :: String, savePayment :: Boolean ) ))
             (Variant ( thankYou :: String, failure :: String )) )
    ( modal
        :: MyRowToRowProfunctor
             (Variant ( cancel :: Record () ))
             (Variant ( cancelled :: String )) )
  VariantToRecord.do
    statusBar @"thankYou"
    eventLog @"failure"
    statusBar @"cancelled"

confirmPayment
  :: MyRowToRowProfunctor
       (Tuple (Record ( amount :: Int )) (Record ( attempt :: Int )))
       (Either (Variant ( settled :: Record ( amount :: Int ) )) (Record ( attempt :: Int )))
confirmPayment = RecordToVariant.resolve (button @"settled")

runningTotal
  :: MyRowToRowProfunctor
       (Either (Variant ( addItem :: String )) (Record ( total :: Int )))
       (Tuple (Record ( addItem :: String )) (Record ( total :: Int )))
runningTotal = VariantToRecord.retain (statusBar @"addItem")
