module TickerViewModel (closedTicker, fetchQuote, phaseLine, quoteFetchedLine, quoteLine, recordQuote, stepAway, tick, tickLine, tickPeriod, watch) where

import Prelude ((<>), (+), (/), ($), bind, discard, pure, show)

import Data.Array (last, length, snoc)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)

closedTicker :: { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int }
closedTicker = { phase: .away {}, quotes: [], ticks: 0 }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 250.0 }

quoteDelay :: { ms :: Number }
quoteDelay = { ms: 800.0 }

phaseLine :: { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int } -> String
phaseLine { phase } = match
  { away: \_ -> "Away from the desk: the board is parked"
  , watching: \_ -> "Watching the market"
  } phase

tickLine :: { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int } -> String
tickLine { ticks } = show ticks <> " ticks while watching"

quoteLine :: { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int } -> String
quoteLine { quotes } = show (length quotes) <> " quotes received" <> maybe "" (\p -> ", last " <> show p) (last quotes)

watch
  :: { event :: {}, model :: { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int } }
  -> { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int }
watch { model } = model { phase = .watching {} }

stepAway
  :: { event :: {}, model :: { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int } }
  -> { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int }
stepAway { model } = model { phase = .away {} }

tick
  :: { event :: {}, model :: { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int } }
  -> { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int }
tick { model } = model { ticks = model.ticks + 1 }

fetchQuote :: {} -> Aff [ "Quote fetched" :: { price :: Number } ]
fetchQuote _ = do
  delay (Milliseconds quoteDelay.ms)
  cents <- liftEffect $ randomInt 9000 11000
  pure $ ."Quote fetched" { price: toNumber cents / 100.0 }

recordQuote
  :: { event :: { price :: Number }, model :: { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int } }
  -> { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int }
recordQuote { event: { price }, model } = model { quotes = snoc model.quotes price }

quoteFetchedLine :: { phase :: [ away :: {}, watching :: {} ], quotes :: Array Number, ticks :: Int } -> String
quoteFetchedLine { quotes } = "Quote received" <> maybe "" (\p -> ": " <> show p) (last quotes)
