module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor.Sum as View
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Flow
import UI (UI)
import Web (Web, body, staticText, text)
import Data.Whatever (Whatever, whateverFirstName)

main :: Effect Unit
main = body design 

design :: UI Web Whatever Void
design = Flow.do
  View.do 
    staticText "Welcome, "
    whateverFirstName $ text
    staticText ". do you want to enter?"
    MDC.containedButton { icon: Nothing, label: Just "Yes" }
  MDC.snackbar $ staticText "Great!"
  staticText "Thank you for using Bambik!"
