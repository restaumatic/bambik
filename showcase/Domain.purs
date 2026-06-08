-- | The domain model for the optics showcase: a tiny **order** application.
-- |
-- | Nothing here is special — just `Record`s (products, all fields at once) and
-- | `Variant`s (sums, one case at a time). The interesting part is
-- | [Showcase.Logic](./Logic.purs), where the *entire* business logic is expressed
-- | as optics over these types. The DDD reading of each declaration is in the
-- | comments: **value objects** (timeless data), the **entity / aggregate** (state
-- | with identity over time), and the **events** it folds in.
module Showcase.Domain where

import Data.Variant (Variant)

-- | Money in minor units (cents). A primitive **value object**.
type Money = Int

-- | **Value objects** — defined wholly by their attributes, no identity, immutable.
-- | These are the `Lens`/`Prism` realm: pure spatial navigation of frozen data.
type Address  = { street :: String, city :: String, zip :: String }
type Customer = { name :: String, email :: String, address :: Address }
type Line     = { sku :: String, qty :: Int, price :: Money }

-- | A closed set of cases — a **value-object union** discriminated by tag.
type CardDetails = { number :: String, expiry :: String }
type Payment = Variant ( card :: CardDetails, cash :: Money, voucher :: String )

-- | The **aggregate root** — an *Entity*, identified by `ref`, carrying its lines
-- | and a `total` it keeps consistent. This is the `Reel` realm: state that
-- | persists and accumulates over time.
type Order =
  { ref      :: String
  , customer :: Customer
  , lines    :: Array Line
  , total    :: Money
  }

-- | The **command channel** into the Order aggregate — the events the entity folds
-- | in. `restore` rehydrates the aggregate from a snapshot (the `Reel` resume branch).
type OrderCmd = Variant ( addLine :: Line, restore :: Order )

-- | The terminal outcome of the checkout **process** (a Saga): it either *completed*
-- | (`Placed`) or *escaped* with its in-flight state (`SavedDraft`). This is the
-- | `Shutter` realm: a step that finishes or loops.
data Submission = Placed { ref :: String } | SavedDraft { draftId :: String }
