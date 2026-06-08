-- | The model for the optics showcase: a tiny checkout flow.
-- |
-- | Just `Record`s (products — all fields at once) and, downstream, `Variant`s
-- | (sums — one case at a time). The business logic lives in
-- | [Showcase.Logic](./Logic.purs), expressed entirely as optics over these types.
module Showcase.Domain where

-- | Money in minor units (cents) — a primitive **value object**.
type Money = Int

-- | The input **form** — a value-object record (all fields present at once).
type Form = { email :: String, amount :: Money }

-- | The rendered **display** — a projection produced from the processed channels.
type Display = { contactNote :: String, chargeNote :: String }
