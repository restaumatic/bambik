-- | **A reactive checkout form, as pure optics.**
-- |
-- | No UI, no effects, no carrier: `p` stays abstract, so the logic is
-- | carrier-independent (see `doc/row-profunctors.md`). The natural unit is the **field**:
-- | the form is a *merge of field widgets*, and each `textInput` widget runs its own
-- | edit → change → validate → status flow over its single field — touching all four
-- | optic families along the way:
-- |
-- |   * **Lens**    (× → ×) — edit the field in place        ("has-a")
-- |   * **Shutter** (× → +) — fire the field's change event  (Process / Saga)
-- |   * **Prism**   (+ → +) — validate that event            ("is-a")
-- |   * **Reel**    (+ → ×) — write the field's status       (Entity / Aggregate)
-- |
-- | The form then flows into a **submit `button`** that fires the whole form as one
-- | `submit` event. No inline annotations: closed rows (`Cons l a () r`) pin every step.
module Showcase.Logic where

import Prelude

import Data.Either (Either(..), either)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord, editProperty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (class Resolving, shutterE)
import Data.Profunctor.Row.VariantToRecord (class Retaining, reelE)
import Data.Profunctor.Row.VariantToVariant (editCase)
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Semigroupoid
import Record (get, insert)
import Type.Proxy (Proxy(..))

-- | A complete **`textInput`** widget for field `l`: a `Semigroupoid.do` (`>>>`) flow
-- | that edits the field, fires its change event, validates it, and writes its status —
-- | one optic family per step, all over the single field `l`:
-- |
-- | ```
-- |   Record ──Lens──▶ Record ──Shutter──▶ Variant ──Prism──▶ Variant ──Reel──▶ Record
-- |     (edit l)         (l changed)        (validate l)        (status l)
-- | ```
textInput
  :: forall @l p a r
   . Category p
  => Strong p
  => Choice p
  => Resolving p
  => Retaining p
  => IsSymbol l
  => Cons l a () r
  => p (Record r) (Record r)
textInput = Semigroupoid.do
  editProperty @l identity                                                            -- × → ×  edit
  shutterE (\rec -> Tuple (get (Proxy @l) rec) (get (Proxy @l) rec))                   -- × → +  change
           (either (inj (Proxy @l)) (inj (Proxy @l))) identity
  editCase @l identity                                                                -- + → +  validate
  reelE (\v -> Left ((case_ # on (Proxy @l) identity) v))                             -- + → ×  status
        (\(Tuple b (_ :: Unit)) -> insert (Proxy @l) b {}) identity

-- | A **submit `button`** (× → +): read the whole form record and fire it as the single
-- | event case `l`, carrying the form as payload (the `Example.purs` `button` shape).
button
  :: forall @l p r v
   . Category p
  => Resolving p
  => IsSymbol l
  => Cons l (Record r) () v
  => p (Record r) (Variant v)
button =
  shutterE
    (\rec -> Tuple rec rec)
    (either (inj (Proxy @l)) (inj (Proxy @l)))
    identity

-- | The form body: one `RecordToRecord.do` merge of the field widgets — each field
-- | listed once, its whole edit/validate/status lifecycle inside its own `textInput`.
form
  :: forall p
   . Category p
  => Strong p
  => Choice p
  => Resolving p
  => Retaining p
  => RecordToRecord p
  => p (Record ( email :: String, cardNumber :: String, amount :: Int ))
       (Record ( email :: String, cardNumber :: String, amount :: Int ))
form = RecordToRecord.do
  textInput @"email"
  textInput @"cardNumber"
  textInput @"amount"

-- | The whole checkout: fill the `form`, then `submit` it. The form flows (`>>>`) into
-- | the submit button, which fires the completed form as a single `submit` event.
checkout
  :: forall p
   . Category p
  => Strong p
  => Choice p
  => Resolving p
  => Retaining p
  => RecordToRecord p
  => p (Record ( email :: String, cardNumber :: String, amount :: Int ))
       (Variant ( submit :: Record ( email :: String, cardNumber :: String, amount :: Int ) ))
checkout = Semigroupoid.do
  form
  button @"submit"
