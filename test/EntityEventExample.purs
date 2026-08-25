module EntityEventExample where

import Prelude

import Data.Lens (Lens', Prism', preview, review, set, view)
import Data.Maybe (Maybe)
import Data.Profunctor.Row.RecordToRecord (field)
import Data.Profunctor.Row.VariantToVariant (focusCase)
import Data.Variant (case_, on) as Variant
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

qtyField :: Lens' OrderEntity Int     -- a field of the entity   (field)
qtyField = field @"qty"

qtyCase :: Prism' OrderEvent Int      -- a case  of the event    (focusCase)
qtyCase = focusCase @"qty"

-- entity -> event : read the live field, fire it as that case
--   (value-level essence of backgroundProperty / Shutter, the × → + leg)
fieldToEvent :: OrderEntity -> OrderEvent
fieldToEvent o = review qtyCase (view qtyField o)

-- event -> entity : fold one event-case back into the held entity
--   (value-level essence of backgroundCase / Reel, the + → × leg)
applyEvent :: OrderEvent -> OrderEntity -> OrderEntity
applyEvent e o =
  ( Variant.case_
      # Variant.on (Proxy @"customer") (\c -> o { customer = c })
      # Variant.on (Proxy @"item") (\i -> o { item = i })
      # Variant.on (Proxy @"qty") (\q -> o { qty = q })
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
