-- | A **Reel** (`+ → ×`) in a restaurant domain: fold a stream of order *events*
-- | (a `Variant`, one case at a time) into a running *order* (a `Record`, carried
-- | forward and never finished).
-- |
-- | The fix over the earlier sketch: there is no free-floating `current` order.
-- | `dispatch :: OrderEvent -> Either a (b -> t)` only sees the event. So the
-- | retained order must enter through the channel the Reel actually exposes —
-- | the residual `c`, which `reel` fixes to the finisher `b -> t`
-- | ([VariantToRecord.purs:100](extras/row-profunctor/Data/Profunctor/Row/VariantToRecord.purs#L100)).
-- |
-- | That makes the protocol a genuine two-beat (exactly the `Right` = "resume
-- | from state" / `Left` = "fresh input" reading in `retain`'s docstring):
-- |
-- |   * `openOrder o` → `Right` : install a finisher that closes over `o` — this
-- |                               *is* the retained state, held in the carrier.
-- |   * `pickDish id` → `Left`  : a fresh focus handed to `priceDish`; when it
-- |                               emits a `PricedLine`, the installed finisher
-- |                               folds it into the retained order.
module RestaurantReel where

import Prelude

import Data.Either (Either(..))
import Data.Profunctor (rmap)
import Data.Lens.Reel (reel)
import Data.Variant (case_, on)
import Type.Proxy (Proxy(..))
import PUI (PUI)
import PUI.Web.HTML (button, staticText)
import PUI.Web (Web)

type Money = Int
type DishId = String
type PricedLine = { name :: DishId, price :: Money }
type Order = { lines :: Array PricedLine, total :: Money }

-- | One event at a time (sum / `Variant`).
type OrderEvent =
  [ openOrder :: Order    -- resume: this order becomes the retained state
  , pickDish  :: DishId   -- focus: a dish to be priced and folded in
  ]

-- | The inner transformer `p a b`: a fresh `DishId` in, a `PricedLine` out
-- | (toy menu lookup; a real one would be an interactive picker with modifiers).
priceDish :: PUI Web DishId PricedLine
priceDish =
  rmap (\id -> { name: id, price: 1000 })
    (button $ staticText "Price dish")

-- | The Reel: events folded into the running order. The order is real retained
-- | state — it lives in the finisher's closure inside the carrier, installed by
-- | `openOrder` and applied when `priceDish` yields a line.
orderReel :: PUI Web OrderEvent Order
orderReel = reel dispatch priceDish
  where
  dispatch :: OrderEvent -> Either DishId (PricedLine -> Order)
  dispatch =
    case_
      # on (Proxy @"openOrder")
          (\order -> Right \line ->
              order { lines = order.lines <> [ line ]
                    , total = order.total + line.price })
      # on (Proxy @"pickDish") Left

-- | Sample events, to read the two-beat at the call site.
beginOrder :: OrderEvent
beginOrder = .openOrder { lines: [], total: 0 }

addEspresso :: OrderEvent
addEspresso = .pickDish "Espresso"
