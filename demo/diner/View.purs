module Diner.View (diner) where

import Prelude ((#), ($), Unit)

import Diner (openService)
import Diner.View.Service (service)
import Effect (Effect)
import PUI (capture, landed, load, looped)
import PUI.Web.MDC3 (body, indeterminateLinearProgress)
import QualifiedDo.Semigroupoid as Semigroupoid

diner :: Effect Unit
diner =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      indeterminateLinearProgress # load openService
      capture @"Serwis otwarty" ) # landed
    service # looped
