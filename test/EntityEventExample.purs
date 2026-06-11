module EntityEventExample where

import Prelude

import Data.Lens (Lens', Prism', preview, review, set, view)
import Data.Maybe (Maybe)
import Data.Profunctor.Row.RecordToRecord (editProperty)
import Data.Profunctor.Row.VariantToVariant (editCase)
import Data.Variant (case_, on)
import Type.Proxy (Proxy(..))

-- ONE row: the order's fields.
type Order = ( customer :: String, item :: String, qty :: Int )

-- The SAME row, two canonical readings:
--   entity = all fields at once     (product / Record)
--   event  = exactly one field fired (sum     / Variant)
type OrderEntity = Record (Order)
type OrderEvent = [ | Order ]

sampleOrder :: OrderEntity
sampleOrder = { customer: "Ada", item: "Espresso", qty: 1 }

-- The SAME label "qty" reads two ways on the same row ----------------------

qtyField :: Lens' OrderEntity Int     -- a field of the entity   (editProperty)
qtyField = editProperty @"qty"

qtyCase :: Prism' OrderEvent Int      -- a case  of the event    (editCase)
qtyCase = editCase @"qty"

-- entity -> event : read the live field, fire it as that case
--   (value-level essence of resolveProperty / Shutter, the × → + leg)
fieldToEvent :: OrderEntity -> OrderEvent
fieldToEvent o = review qtyCase (view qtyField o)

-- event -> entity : fold one event-case back into the held entity
--   (value-level essence of retainCase / Reel, the + → × leg)
applyEvent :: OrderEvent -> OrderEntity -> OrderEntity
applyEvent e o =
  ( case_
      # on (Proxy @"customer") (\c -> o { customer = c })
      # on (Proxy @"item") (\i -> o { item = i })
      # on (Proxy @"qty") (\q -> o { qty = q })
  ) e

-- the two readings, used independently
setQty :: Int -> OrderEntity -> OrderEntity
setQty = set qtyField

readQty :: OrderEvent -> Maybe Int
readQty = preview qtyCase

-- A worked trace (values that result):
--   sampleOrder                          = { customer: "Ada", item: "Espresso", qty: 1 }
--   fieldToEvent sampleOrder             = inj @"qty" 1        -- the qty field, fired as an event
--   applyEvent (review qtyCase 3) order  = order { qty = 3 }   -- that event folded back in
--   readQty (review qtyCase 7)           = Just 7
