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
-- | There are no inline annotations: the widget's closed row (`Cons l a () r`) pins every
-- | step to the single field/case `l`, and the field types unify from `checkoutFlow`.
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
import Data.Variant (case_, inj, on)
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

-- | The checkout form: one `RecordToRecord.do` merge of the field widgets. The smelly
-- | four-do-blocks-over-the-same-fields pipeline is gone — each field appears once, and
-- | its whole edit/validate/status lifecycle lives inside its own `textInput`.
checkoutFlow
  :: forall p
   . Category p
  => Strong p
  => Choice p
  => Resolving p
  => Retaining p
  => RecordToRecord p
  => p (Record ( email :: String, cardNumber :: String, amount :: Int ))
       (Record ( email :: String, cardNumber :: String, amount :: Int ))
checkoutFlow = RecordToRecord.do
  textInput @"email"
  textInput @"cardNumber"
  textInput @"amount"
