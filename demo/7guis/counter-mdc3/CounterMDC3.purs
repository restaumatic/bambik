module CounterMDC3 (counterMDC3) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, mvu)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, button, headlineLarge)
import QualifiedDo.Semigroupoid as Semigroupoid

counterMDC3 :: Effect Unit
counterMDC3 =
  body $
    ( Semigroupoid.do
      -- ×→× record to record: the display. Fed the model row { count }, it renders
      -- countLine of it and, being a display, releases the same row to the next stage.
      headlineLarge (text countLine) # shown
      -- ×→+ record to variant: the emitter. Fed the row, it shows nothing new and emits
      -- nothing; on a click it replays the row it was last fed as the event [ "Count" :: { count } ].
      button @"Count" {}
      -- +→× variant to record: the fold. Owns the case "Count", applies increment to its
      -- payload — the row the button was fed, so the model is already in hand — and
      -- releases the result as the record { count } again.
      fold @"Count" increment
    -- mvu loops the record released at the bottom back to the top, so the display re-renders
    -- and the button retains the new row for its next click. The model row is declared here,
    -- once, on the seed line; freshCount is the row fed first.
    ) # mvu @( count :: Int ) freshCount
