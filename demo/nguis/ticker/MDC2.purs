module Ticker.MDC2 (ticker) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, fold, joined, looped, ticks, unshown, with)
import PUI.Web (providedAt, text)
import PUI.Web.MDC2 (body, body1, button', headline6, indeterminateLinearProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import Ticker (closedTicker, fetchQuote, phaseLine, quoteFetchedLine, quoteLine, recordQuote, stepAway, tick, tickLine, tickPeriod, watch)

ticker :: Effect Unit
ticker =
  body $ Semigroupoid.do
    headline6 (text phaseLine)
    body1 (text tickLine)
    body1 (text quoteLine)
    RecordToVariant.do
      button' @"Watch" { icon: "visibility" } # providedAt @"away" @"phase"
      button' @"Step away" { icon: "visibility_off" } # providedAt @"watching" @"phase"
      unshown @"Ticked" # ticks tickPeriod # providedAt @"watching" @"phase"
      ( Semigroupoid.do
        button' @"Fetch quote" { icon: "download" }
        indeterminateLinearProgress # action @( "Quote fetched" :: { price :: Number } ) fetchQuote # atCase @"Fetch quote" ) # providedAt @"watching" @"phase" # joined @"Quote fetched"
    VariantToRecord.do
      unshown @"Watch" # fold watch
      unshown @"Step away" # fold stepAway
      unshown @"Ticked" # fold tick
      snackbar @"Quote fetched" quoteFetchedLine # fold recordQuote
  # looped @( phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int ) # with closedTicker
