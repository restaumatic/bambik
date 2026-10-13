module TickerMDC2 (tickerMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, fold, joined, looped, prefixed, ticks, unshown, with)
import PUI.Web (providedAt, text)
import PUI.Web.MDC2 (body, body1, buttonWith, headline6, indeterminateLinearProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import TickerViewModel (closedTicker, fetchQuote, phaseLine, quoteFetchedLine, quoteLine, recordQuote, stepAway, tick, tickLine, tickPeriod, watch)

tickerMDC2 :: Effect Unit
tickerMDC2 =
  body $ Semigroupoid.do
    prefixed ( RecordToRecord.do
      headline6 (text phaseLine)
      body1 (text tickLine)
      body1 (text quoteLine) ) $ RecordToVariant.do
      buttonWith @"Watch" { icon: "visibility" } # providedAt @"away" @"phase" # joined @"Watch"
      buttonWith @"Step away" { icon: "visibility_off" } # providedAt @"watching" @"phase" # joined @"Step away"
      unshown @"Ticked" # ticks tickPeriod # providedAt @"watching" @"phase" # joined @"Ticked"
      ( Semigroupoid.do
        buttonWith @"Fetch quote" { icon: "download" }
        indeterminateLinearProgress # action @( "Quote fetched" :: { price :: Number } ) fetchQuote # atCase @"Fetch quote" ) # providedAt @"watching" @"phase" # joined @"Quote fetched"
    VariantToRecord.do
      unshown @"Watch" # fold watch
      unshown @"Step away" # fold stepAway
      unshown @"Ticked" # fold tick
      snackbar @"Quote fetched" quoteFetchedLine # fold recordQuote
  # looped @( phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int ) # with closedTicker
