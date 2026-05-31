module Data.Profunctor.Row.Default
  ( withRecordDefault
  , withRecordOutputDefault
  , tagVariantInput
  , tagVariantOutput
  )
  where

import Prelude (identity, (#))

import Data.Function (const)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, case_, inj, on)
import Prim.Row (class Cons)
import Prim.RowList as RL
import Record (insert)
import Type.Proxy (Proxy(..))

-- UI: seed a single-field input with an initial value. A widget that needs
-- a record field to display (e.g. `textInput @"name"`) becomes one needing
-- no input data — the default is shown initially and user edits flow back
-- via `o`. The default is consumed on every render.
-- Lifts `p (Record (l :: a)) o` into `p (Record ()) o`.
withRecordDefault :: forall l p a r o.
  RL.RowToList r (RL.Cons l a RL.Nil) =>
  IsSymbol l =>
  Cons l a () r =>
  Profunctor p =>
  p (Record r) o -> a -> p (Record ()) o -- can be placed in any record-to-*
withRecordDefault p default = lcmap (const (insert (Proxy :: Proxy l) default {})) p

-- UI: promote a read-only widget into a form contributor. A display-only
-- element like `textOutput` or `icon` that captures nothing gets lifted to
-- one that emits a fixed singleton record on every call — useful for static
-- fields like auto-IDs, hidden constants, or computed values the form layer
-- needs. The default is emitted on every render.
-- Lifts `p i (Record ())` into `p i (Record (l :: a))`.
withRecordOutputDefault :: forall l p a r i.
  RL.RowToList r (RL.Cons l a RL.Nil) =>
  IsSymbol l =>
  Cons l a () r =>
  Profunctor p =>
  p i (Record ()) -> a -> p i (Record r) -- can be placed in any *-to-record
withRecordOutputDefault p default = rmap (const (insert (Proxy :: Proxy l) default {})) p

-- UI: re-shape a raw-value consumer as a single-case variant consumer.
-- A widget that displays a raw `a` (e.g. a notification taking a `String`)
-- becomes one that displays the `l` case of a wider event variant — the
-- case payload is unwrapped and routed to the underlying widget.
-- Symmetric to `withRecordDefault` but takes only a label: the singleton
-- variant `Variant (l :: a)` already carries an `a`, so no value is needed.
-- Lifts `p a o` into `p (Variant (l :: a)) o`.
tagVariantInput :: forall l p a r o.
  RL.RowToList r (RL.Cons l a RL.Nil) =>
  IsSymbol l =>
  Cons l a () r =>
  Profunctor p =>
  p a o -> p (Variant r) o -- can be placed in any variant-to-*
tagVariantInput p = lcmap (case_ # on (Proxy :: Proxy l) identity) p

-- UI: re-shape a raw-value producer as a single-case variant producer.
-- A widget that emits a raw `a` (e.g. a button emitting click time as
-- `Number`) becomes one that emits the `l` case of a wider event variant —
-- the produced value is tagged on the way out.
-- Symmetric to `withRecordOutputDefault` but takes only a label: nothing
-- needs to be padded since the value comes from `p` itself.
-- Lifts `p i a` into `p i (Variant (l :: a))`.
tagVariantOutput :: forall l p a r i.
  RL.RowToList r (RL.Cons l a RL.Nil) =>
  IsSymbol l =>
  Cons l a () r =>
  Profunctor p =>
  p i a -> p i (Variant r) -- can be placed in any *-to-variant
tagVariantOutput p = rmap (inj (Proxy :: Proxy l)) p
