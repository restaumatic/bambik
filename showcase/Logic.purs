-- | **The checkout app's business logic, as one pipeline of optics — no annotations.**
-- |
-- | No UI, no effects, no carrier: `p` stays abstract, so the logic is
-- | carrier-independent (see `doc/row-profunctors.md`). The four optics map onto
-- | Domain-Driven Design's tactical vocabulary, and each is one stage of the flow:
-- |
-- |   * **Lens**    (× → ×) — value-object field access  ("has-a")    — `onField`
-- |   * **Shutter** (× → +) — the Process / Saga                       — `fieldToCase`
-- |   * **Prism**   (+ → +) — value-object case match     ("is-a")     — `onCase`
-- |   * **Reel**    (+ → ×) — the Entity / Aggregate                   — `caseToField`
-- |
-- | There are no inline `:: p (…) (…)` annotations: each leaf is a **closed-row**
-- | combinator (`Cons l a () r` pins the row to the single field/case `l`), so the merges
-- | can split the form unambiguously without a call-site signature; the field types are
-- | then unified from `checkoutFlow`'s endpoints. This is exactly the idiom of
-- | `Data.Profunctor.Row.Example`'s widget leaves (`textInput @l`, `button @l`, …) — a
-- | label plus a closed single-field/case row — here generalized over the carrier `p`.
module Showcase.Logic where

import Prelude

import Data.Either (Either(..), either)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord, editProperty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant, class Resolving, shutterE)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord (class Retaining, class VariantToRecord, reelE)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant, editCase)
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Semigroupoid
import Record (get, insert)
import Type.Proxy (Proxy(..))

-- | **Lens** leaf (× → ×): focus field `l` in place. Closed row → no annotation needed.
onField :: forall @l p a r. Category p => Strong p => IsSymbol l => Cons l a () r => p (Record r) (Record r)
onField = editProperty @l identity

-- | **Shutter** leaf (× → +): lift field `l` into output case `l`.
fieldToCase :: forall @l p a r. Category p => Resolving p => IsSymbol l => Cons l a () r => p (Record r) (Variant r)
fieldToCase =
  shutterE
    (\rec -> Tuple (get (Proxy @l) rec) (get (Proxy @l) rec))
    (either (inj (Proxy @l)) (inj (Proxy @l)))
    identity

-- | **Prism** leaf (+ → +): route case `l` in place.
onCase :: forall @l p a r. Category p => Choice p => IsSymbol l => Cons l a () r => p (Variant r) (Variant r)
onCase = editCase @l identity

-- | **Reel** leaf (+ → ×): render case `l` into output field `l`.
caseToField :: forall @l p a r. Category p => Retaining p => IsSymbol l => Cons l a () r => p (Variant r) (Record r)
caseToField =
  reelE
    (\v -> Left ((case_ # on (Proxy @l) identity) v))
    (\(Tuple b (_ :: Unit)) -> insert (Proxy @l) b {})
    identity

-- | The whole checkout, as one `Semigroupoid.do` (`>>>`) pipeline. Each merge do-block
-- | merges that stage's two closed-row leaves, and the outer `Semigroupoid.do` flows the
-- | four stages together — no type annotations anywhere in the body:
-- |
-- | ```
-- |   Record  ──Lens──▶  Record  ──Shutter──▶  Variant  ──Prism──▶  Variant  ──Reel──▶  Record
-- |   RecordToRecord.do    RecordToVariant.do     VariantToVariant.do     VariantToRecord.do
-- | ```
checkoutFlow
  :: forall p
   . Category p
  => Strong p
  => Choice p
  => Resolving p
  => Retaining p
  => RecordToRecord p
  => RecordToVariant p
  => VariantToVariant p
  => VariantToRecord p
  => p (Record ( email :: String, amount :: Int )) (Record ( email :: String, amount :: Int ))
checkoutFlow = Semigroupoid.do
  RecordToRecord.do      -- × → ×   Lens: normalize each field
    onField @"email"
    onField @"amount"
  RecordToVariant.do     -- × → +   Shutter: lift each field into a channel
    fieldToCase @"email"
    fieldToCase @"amount"
  VariantToVariant.do    -- + → +   Prism: route each channel
    onCase @"email"
    onCase @"amount"
  VariantToRecord.do     -- + → ×   Reel: render each channel back into a field
    caseToField @"email"
    caseToField @"amount"
