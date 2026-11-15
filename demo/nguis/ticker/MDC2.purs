module Ticker.MDC2 (ticker) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, fold, looped, stepped, ticks, with)
import PUI.Web (inCaseAt, providedAt, text)
import PUI.Web.MDC2 (body, body1, button', headline6, indeterminateLinearProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import Ticker (closedTicker, fetchQuote, phaseLine, quoteLine, recordQuote, stepAway, tick, tickLine, tickPeriod, watch)

ticker :: Effect Unit
ticker =
  body $ Semigroupoid.do
    headline6 $ text phaseLine
    body1 $ text tickLine
    body1 $ text quoteLine
    ( Semigroupoid.do
      RecordToVariant.do
        button' @"Watch" { icon: "visibility" } # providedAt @"away" @"phase"
        button' @"Step away" { icon: "visibility_off" } # providedAt @"watching" @"phase"
        ticks @"Clock ticked" tickPeriod # providedAt @"watching" @"phase"
      VariantToRecord.do
        fold @"Watch" watch
        fold @"Step away" stepAway
        fold @"Clock ticked" tick ) # stepped
    ( Semigroupoid.do
      button' @"Fetch quote" { icon: "download" }
      indeterminateLinearProgress # action @"Fetch quote" fetchQuote
      snackbar @"Quote fetched" recordQuote ) # stepped # inCaseAt @"watching" @"phase"
  # looped # with closedTicker
