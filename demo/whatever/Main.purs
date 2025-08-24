module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor.Endo as Form
import Data.Profunctor.Sum as View
import Data.Whatever (Whatever, byDefault, whateverFirstName)
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Flow
import UI (UI)
import Web (Web, body, staticText, text)

main :: Effect Unit
main = body subscription

subscription :: UI Web Whatever Void
subscription = Flow.do
  View.do 
    staticText "Welcome, "
    whateverFirstName $ text -- `whateverFirstName` -> `firstName :: Iso s s String Void`
    staticText ". do you want to subscribe?"
    MDC.containedButton { icon: Nothing, label: Just "Yes" }
  MDC.simpleDialog { title: "Subscription", confirm: "Subscribe"} Form.do
    staticText "Please enter your data:"
    byDefault "" $ MDC.filledTextField { floatingLabel: "Phone Number" } -- `byDefault` -> `phoneNumber :: String -> Lens' s String`
    byDefault "" $ MDC.filledTextField { floatingLabel: "Address" } -- `byDefault ""` -> `address :: String -> Lens' s String`
  MDC.snackbar $ staticText "Great!"
