-- | **The checkout app's business logic, as one pipeline of optics — no annotations.**
-- |
-- | No UI, no effects, no carrier: `p` stays abstract, so the logic is
-- | carrier-independent (see `doc/row-profunctors.md`). The four optics map onto
-- | Domain-Driven Design's tactical vocabulary, and each is one stage of the flow:
-- |
-- |   * **Lens**    (× → ×) — value-object field access  ("has-a")    — `textInput`
-- |   * **Shutter** (× → +) — the Process / Saga                       — `button`
-- |   * **Prism**   (+ → +) — value-object case match     ("is-a")     — `notification`
-- |   * **Reel**    (+ → ×) — the Entity / Aggregate                   — `statusBar`
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

-- | **`textInput`** — a **Lens** leaf (× → ×): show and edit field `l` in place.
-- | Closed row (`Cons l a () r`) → no annotation needed.
textInput :: forall @l p a r. Category p => Strong p => IsSymbol l => Cons l a () r => p (Record r) (Record r)
textInput = editProperty @l identity

-- | **`button`** — a **Shutter** leaf (× → +): read field `l` from the model, fire it as case `l`.
button :: forall @l p a r. Category p => Resolving p => IsSymbol l => Cons l a () r => p (Record r) (Variant r)
button =
  shutterE
    (\rec -> Tuple (get (Proxy @l) rec) (get (Proxy @l) rec))
    (either (inj (Proxy @l)) (inj (Proxy @l)))
    identity

-- | **`notification`** — a **Prism** leaf (+ → +): react to case `l` and re-emit it.
notification :: forall @l p a r. Category p => Choice p => IsSymbol l => Cons l a () r => p (Variant r) (Variant r)
notification = editCase @l identity

-- | **`statusBar`** — a **Reel** leaf (+ → ×): display case `l` as field `l`.
statusBar :: forall @l p a r. Category p => Retaining p => IsSymbol l => Cons l a () r => p (Variant r) (Record r)
statusBar =
  reelE
    (\v -> Left ((case_ # on (Proxy @l) identity) v))
    (\(Tuple b (_ :: Unit)) -> insert (Proxy @l) b {})
    identity

-- | A reactive **checkout screen**, as one `Semigroupoid.do` (`>>>`) pipeline. The
-- | shopper's form (`email`, `cardNumber`, `amount`) flows through four stages — one
-- | per optic family — and the same data lands back as a status line. Each merge
-- | do-block wires that stage's three field widgets; the outer `Semigroupoid.do` flows
-- | the stages. No type annotations anywhere in the body:
-- |
-- | ```
-- |   form  ──textInput──▶  form  ──button──▶  events  ──notification──▶  events  ──statusBar──▶  status
-- |   RecordToRecord.do       RecordToVariant.do      VariantToVariant.do        VariantToRecord.do
-- |    (edit each field)       (submit → event)        (validate → notice)        (event → status)
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
  => p (Record ( email :: String, cardNumber :: String, amount :: Int ))
       (Record ( email :: String, cardNumber :: String, amount :: Int ))
checkoutFlow = Semigroupoid.do
  RecordToRecord.do      -- × → ×   the form: an editable input per field
    textInput @"email"
    textInput @"cardNumber"
    textInput @"amount"
  RecordToVariant.do     -- × → +   submit: each field fires a change event
    button @"email"
    button @"cardNumber"
    button @"amount"
  VariantToVariant.do    -- + → +   validate: a notification per event
    notification @"email"
    notification @"cardNumber"
    notification @"amount"
  VariantToRecord.do     -- + → ×   status: render each event into the status line
    statusBar @"email"
    statusBar @"cardNumber"
    statusBar @"amount"
