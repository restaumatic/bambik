module Ticker (Quote, Ticker, closedTicker, fetchQuote, phaseLine, quoteLine, recordQuote, stepAway, tick, tickLine, tickPeriod, watch) where

import Prelude ((<>), (+), (/), ($), bind, discard, pure, show)

import Data.Array (last, length, snoc)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)

type Ticker = { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int }

type Quote = { price :: Number }

closedTicker :: Ticker
closedTicker = { phase: .away {}, quotes: [], ticks: 0 }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 250.0 }

quoteDelay :: { ms :: Number }
quoteDelay = { ms: 800.0 }

phaseLine :: Ticker -> String
phaseLine { phase } = match
  { away: \_ -> "Away from the desk: the board is parked"
  , watching: \_ -> "Watching the market"
  } phase

tickLine :: Ticker -> String
tickLine { ticks } = show ticks <> " ticks while watching"

quoteLine :: Ticker -> String
quoteLine { quotes } = show (length quotes) <> " quotes received" <> maybe "" (\p -> ", last " <> show p) (last quotes)

watch :: Ticker -> Ticker
watch model = model { phase = .watching {} }

stepAway :: Ticker -> Ticker
stepAway model = model { phase = .away {} }

tick :: Ticker -> Ticker
tick model = model { ticks = model.ticks + 1 }

fetchQuote :: Ticker -> Aff [ "Quote fetched" :: Quote ]
fetchQuote _ = do
  delay (Milliseconds quoteDelay.ms)
  cents <- liftEffect $ randomInt 9000 11000
  pure $ ."Quote fetched" { price: toNumber cents / 100.0 }

recordQuote :: { event :: Quote, model :: Ticker } -> Ticker
recordQuote { event: { price }, model } = model { quotes = snoc model.quotes price }

