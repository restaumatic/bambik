module CounterMDC3 (counterMDC3) where

import Prelude ((#), ($), Unit)

import CounterViewModel (countedLine, countLine, freshCount, increment)
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, button, headlineLarge, snackbar)
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
      -- +→× variant to record: the status opening the fold. The snackbar is a +→× citizen
      -- owning the case "Count": it renders countedLine of each occurrence and releases no
      -- field of its own; fold puts the wire beside it, so increment has the model in hand
      -- (the payload is the row the button was fed), the fold retains nothing and releases
      -- the record { count } again. A loop with several events has one such status-opened
      -- fold per case, merged in a VariantToRecord.do block.
      snackbar @"Count" countedLine # fold increment
    -- looped feeds the record released at the bottom back to the top, so the display re-renders
    -- and the button retains the new row for its next click; with closes the loop by feeding it
    -- first. The model row is declared here, once, on the seed line.
    ) # looped @( count :: Int ) # with freshCount
